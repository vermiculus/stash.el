;;; cachey.el --- generic, persistent caching        -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: lisp, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Like `pcache', Cachey is implemented in terms of `caches' and
;; `repos'.  A `cache' is a particular piece of data stored
;; optionally by a key or its hash.  A `repo' is a collection of
;; these caches in a file.
;;
;; Any single cache can expire after a set amount of time, at which
;; point a signal is raised to the calling function.  Repos can be
;; written to disk continuously or on a timer.

;;; Code:

(defvar cachey--cache-repo-map   (make-hash-table))
(defvar cachey--cache-timeout-map (make-hash-table))
(defvar cachey--cache-updated-map (make-hash-table))
(defvar cachey--repo-file-map    (make-hash-table))
(defvar cachey--repo-async-map   (make-hash-table))
(defvar cachey--repo-data-map    (make-hash-table))
(defvar cachey--repo-changed-map (make-hash-table))
(defcustom cachey-directory
  (locate-user-emacs-file "caches")
  "Directory in which to save repos.")

(defun cachey-message (msg &rest args)
  (apply #'message (concat "[cachey]: " msg) args))

(define-error
  'cachey-error
  "Cache error")

(define-error
  'cachey-repo-not-found-error
  "Cannot find repo"
  'cachey-error)

(define-error
  'cachey-cache-not-found-error
  "Cannot find cache's repo"
  'cachey-error)

(define-error
  'cachey-cache-not-in-repo-error
  "Cannot find cache in its repo"
  'cachey-error)

(define-error
  'cachey-cache-expired-error
  "Cache has expired"
  'cachey-error)

(defun cachey--read-file (file)
  "Read a file as Lisp data."
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-string))))

(defun cachey-defcache (repo cache &optional timeout initial-value)
  "Define under REPO a CACHE.
TIMEOUT is either nil or the number of seconds after which this
initial-value expires.  If INITIAL-VALUE non-nil, CACHE is set by
`cachey-put'.  Otherwise, the initial value of CACHE is nil."
  (let ((repo-data (gethash repo cachey--repo-data-map
                             'cachey--repo-not-found)))
    (if (eq repo-data 'cachey--repo-not-found)
        (error "Repo `%S' is not defined" repo)
      (puthash cache repo
               cachey--cache-repo-map)
      (puthash cache (if timeout (seconds-to-time timeout))
               cachey--cache-timeout-map)
      (cachey-put cache initial-value)
      cache)))

(defun cachey-defrepo (repo file &optional write-delay read)
  "Define a REPO with associated FILE.
WRITE-DELAY is either nil or the number of seconds after which
REPO is written to disk at FILE.  \(This is done with
`run-with-idle-timer'.)  If READ is non-nil, the repo is read
from FILE instead of created anew."
  (puthash repo (expand-file-name file cachey-directory)
           cachey--repo-file-map)
  (puthash repo (if read (cachey--read-file file)
                   (make-hash-table))
           cachey--repo-data-map)
  (puthash repo write-delay
           cachey--repo-async-map)
  (when write-delay
    (run-with-idle-timer
     write-delay t
     #'cachey-write repo))
  repo)

(defun cachey--get-cache-timeout (cache)
  (gethash cache cachey--cache-timeout-map))

(defun cachey--get-cache-updated (cache)
  (gethash cache cachey--cache-updated-map))

(defun cachey--get-cache-repo (cache)
  (gethash cache cachey--cache-repo-map))

(defun cachey--repo-changed-p (repo)
  (gethash repo cachey--repo-changed-map))

(defun cachey--repo-get-file (repo)
  (gethash repo cachey--repo-file-map))

(defun cachey-cache-expired-p (cache)
  "Return non-nil if CACHE has expired."
  (let ((timeout (cachey--get-cache-timeout cache)))
    (if timeout
        (not
         (time-less-p
          (time-since (cachey--get-cache-updated cache))
          timeout)))))

(defun cachey-get (cache)
  "Return the value of CACHE."
  (let ((repo (gethash cache cachey--cache-repo-map
                        'cachey--cache-not-found)))
    (if (eq repo 'cachey--cache-not-found)
        (signal 'cachey-cache-not-found-error cache)
      (let ((data (gethash repo cachey--repo-data-map
                           'cachey--repo-not-found)))
        (if (eq data 'cachey--repo-not-found)
            (signal 'cachey-repo-not-found-error repo)
          (let ((value (gethash cache data
                                'cachey--cache-not-in-repo)))
            (if (eq value 'cachey--cache-not-in-repo)
                (signal 'cachey-cache-not-in-repo-error cache)
              (if (cachey-cache-expired-p cache)
                  (signal 'cachey-cache-expired-error cache)
                value))))))))

(defun cachey-put (cache value)
  "Update CACHE to VALUE.
If CACHE's associcated repo does not write on a delay, the repo
is written to disk."
  (let* ((repo (gethash cache cachey--cache-repo-map))
         (data (gethash repo cachey--repo-data-map)))
    (puthash cache value data)
    (puthash cache (current-time) cachey--cache-updated-map)
    (puthash repo t cachey--repo-changed-map)
    (when (not (gethash repo cachey--repo-async-map))
      (cachey-write repo))
    value))

(defun cachey-write-all ()
  "Write all repos with `cachey-write'."
  (maphash (lambda (repo _file)
             (cachey-write repo))
           cachey--repo-file-map))

(defun cachey-write (repo)
  "Write REPO to disk if its data has changed."
  (when (cachey--repo-changed-p repo)
    (let ((file (cachey--repo-get-file repo))
          print-length
          print-level)
      (mkdir cachey-directory)
      (write-region
       (prin1-to-string (gethash repo cachey--repo-data-map)) nil
       (cachey--repo-get-file repo))
      (cachey-message "Wrote repo `%S' to %S" repo file)
      (puthash repo nil cachey--repo-changed-map))))

(provide 'cachey)
;;; cachey.el ends here
