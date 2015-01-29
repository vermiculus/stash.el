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

;; Cachey is implemented in terms of `caches' and `stores'.  A `cache'
;; is a particular piece of data stored optionally by a key or its
;; hash.  A `store' is a collection of these caches in a file.
;;
;; Any single cache can expire after a set amount of time, at which
;; point a signal is raised to the calling function.  Stores can be
;; written to disk continuously or on a timer.

;;; Code:

(defvar cachey--cache-store-map   (make-hash-table))
(defvar cachey--cache-timeout-map (make-hash-table))
(defvar cachey--cache-updated-map (make-hash-table))
(defvar cachey--store-file-map    (make-hash-table))
(defvar cachey--store-async-map   (make-hash-table))
(defvar cachey--store-data-map    (make-hash-table))
(defvar cachey--store-changed-map (make-hash-table))
(defcustom cachey-directory
  (locate-user-emacs-file "caches")
  "Directory in which to save stores.")

(defun cachey--read-file (file)
  "Read a file as Lisp data."
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-string))))

(defun cachey-defcache (store cache &optional timeout initial-value)
  "Define under STORE a CACHE.
TIMEOUT is either nil or the number of seconds after which this
initial-value expires.  If INITIAL-VALUE non-nil, CACHE is set by
`cachey-put'.  Otherwise, the initial value of CACHE is nil."
  (let ((store-data (gethash store cachey--store-data-map
                             'cachey--store-not-found)))
    (if (eq store-data 'cachey--store-not-found)
        (error "Store `%S' is not defined" store)
      (puthash cache store
               cachey--cache-store-map)
      (puthash cache (if timeout (seconds-to-time timeout))
               cachey--cache-timeout-map)
      (cachey-put cache initial-value)
      cache)))

(defun cachey-defstore (store file &optional write-delay read)
  "Define a STORE with associated FILE.
WRITE-DELAY is either nil or the number of seconds after which
STORE is written to disk at FILE.  \(This is done with
`run-with-idle-timer'.)  If READ is non-nil, the store is read
from FILE instead of created anew."
  (puthash store file
           cachey--store-file-map)
  (puthash store (if read (cachey--read-file file)
                   (make-hash-table))
           cachey--store-data-map)
  (puthash store write-delay
           cachey--store-async-map)
  (when write-delay
    (run-with-idle-timer
     write-delay t
     #'cachey-write store))
  store)

(defun cachey--get-cache-timeout (cache)
  (gethash cache cachey--cache-timeout-map))

(defun cachey--get-cache-updated (cache)
  (gethash cache cachey--cache-updated-map))

(defun cachey--get-cache-store (cache)
  (gethash cache cachey--cache-store-map))

(defun cachey--store-changed-p (store)
  (gethash store cachey--store-changed-map))

(defun cachey--store-get-file (store)
  (gethash store cachey--store-file-map))

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
  (let ((store (gethash cache cachey--cache-store-map
                        'cachey--cache-not-found)))
    (if (eq store 'cachey--cache-not-found)
        (signal store cache)
      (let ((data (gethash store cachey--store-data-map
                           'cachey--store-not-found)))
        (if (eq data 'cachey--store-not-found)
            (signal data store)
          (let ((value (gethash cache data 'cache-not-in-store)))
            (if (eq value 'cache-not-in-store)
                (signal value cache)
              (if (cachey-cache-expired-p cache)
                  (signal 'cache-expired cache)
                value))))))))

(defun cachey-put (cache value)
  "Update CACHE to VALUE.
If CACHE's associcated store does not write on a delay, the store
is written to disk."
  (let* ((store (gethash cache cachey--cache-store-map))
         (data (gethash store cachey--store-data-map)))
    (puthash cache value data)
    (puthash cache (current-time) cachey--cache-updated-map)
    (puthash store t cachey--store-changed-map)
    (when (not (gethash store cachey--store-async-map))
      (cachey-write store))
    value))

(defun cachey-write-all ()
  "Write all stores with `cachey-write'."
  (maphash (lambda (store _file)
             (cachey-write store))
           cachey--store-file-map))

(defun cachey-write (store)
  "Write STORE to disk if its data has changed."
  (when (cachey--store-changed-p store)
    (let ((file (cachey--store-get-file store))
          print-length
          print-level)
      (mkdir cachey-directory)
      (write-region
       (prin1-to-string (gethash store cachey--store-data-map)) nil
       (expand-file-name (cachey--store-get-file store) cachey-directory))
      (message "Wrote store `%S' to %S" store file)
      (puthash store nil cachey--store-changed-map))))

(provide 'cachey)
;;; cachey.el ends here
