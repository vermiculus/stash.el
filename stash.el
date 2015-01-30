;;; stash.el --- lightweight persistent caching      -*- lexical-binding: t; -*-

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
(defgroup stash nil
  "Customization group for stash."
  :prefix "stash-"
  :group emacs)

(defcustom stash-directory (locate-user-emacs-file "stash")
  "Directory where stash variable files are saved."
  :type 'directory
  :group 'stash)

(defun stash-new (variable file &optional default-value write-delay)
  "Define VARIABLE as a new stash to be written to FILE.
VARIABLE's default value will be DEFAULT-VALUE.  When set, it
will automatically be written to disk after Emacs is idle for
WRITE-DELAY seconds."
  (put variable 'stash-file file)
  (put variable 'stash-default-value default-value)
  (put variable 'stash-write-delay write-delay)
  (stash-set variable default-value))

(defun stash-set (variable value &optional immediate-write)
  "Set VARIABLE to VALUE.
If IMMEDIATE-WRITE is non-nil, VARIABLE's data is written to disk
immediately."
  (set variable value)
  (let ((delay (get variable 'stash-write-delay)))
    (if (and delay (not immediate-write))
        (run-with-idle-timer delay nil #'stash-save variable)
      (stash-save variable)))
  (stash-get variable))

(defun stash-get (variable)
  "Return VARIABLE's data."
  (symbol-value variable))

(defun stash-save (variable)
  "Write VARIABLE's data to disk."
  (write-region
   (let (print-length print-level)
     (prin1-to-string (stash-get variable)))
   nil
   (expand-file-name (get variable 'stash-file)) stash-directory)
  (stash-get variable))

(defun stash-reset (variable)
  "Reset VARIABLE to its initial value."
  (stash-set variable (get variable 'stash-default-value)))

(provide 'stash)
;;; stash.el ends here
