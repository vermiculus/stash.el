;;; stash.el --- lightweight persistent caching

;; Copyright (C) 2015  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: extensions, data, internal, lisp
;; Version: 0.1

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

;; stash.el provides lightweight, persistent caching of Lisp data.  It
;; enables the programmer to create variables which will be written to
;; disk after a certain amount of idle time, as to not cause
;; unnecessary blocks to execution.

;;; Code:

(defun stash-new (variable file &optional default-value write-delay)
  "Define VARIABLE as a new stash to be written to FILE.
VARIABLE's default value will be DEFAULT-VALUE.  When set, it
will automatically be written to disk after Emacs is idle for
WRITE-DELAY seconds."
  (put variable :file file)
  (put variable :default-value default-value)
  (put variable :write-delay write-delay)
  (stash-set variable default-value))

(defun stash-set (variable value &optional immediate-write)
  "Set VARIABLE to VALUE.
If IMMEDIATE-WRITE is non-nil, VARIABLE's data is written to disk
immediately."
  (set variable value)
  (let ((delay (get variable :write-delay)))
    (if (and delay (not immediate-write))
        (run-with-idle-timer delay nil #'stash-save variable)
      (stash-save variable)))
  (stash-get variable))

(defsubst stash-get (variable)
  "Return VARIABLE's data."
  (symbol-value variable))

(defun stash-save (variable)
  "Write VARIABLE's data to disk."
  (write-region
   (let (print-length print-level)
     (prin1-to-string (stash-get variable)))
   nil
   (stash-file variable))
  (stash-get variable))

(defsubst stash-file (variable)
  (get variable :file))

(defun stash-load (variable)
  "Read and set VARIABLE from disk.
If the associated file does not exist, the value of VARIABLE is
reset."
  (let ((file (stash-file variable)))
    (if (file-exists-p file)
        (stash-set
         variable
         (with-temp-buffer
           (insert-file-contents file)
           (read (buffer-string))))
      (stash-reset variable))))

(defun stash-reset (variable)
  "Reset VARIABLE to its initial value."
  (stash-set variable (get variable :default-value)))

(provide 'stash)
;;; stash.el ends here
