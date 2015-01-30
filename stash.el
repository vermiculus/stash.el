;;; stash.el --- lightweight persistent caching      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: extensions, data, internal, lisp

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

;; Lightweight, persistent caching of lisp data

;;; Code:

(defun stash-new (variable file &optional init-value)
  (stash-set variable init-value)
  (put variable :file file)
  (put variable :init-value init-value)
  (symbol-value variable))

(defun stash-set (variable value)
  (set variable value))

(defun stash-save (variable)
  (write-region
   (let (print-length print-level)
     (prin1-to-string (symbol-value variable)))
   nil
   (get variable :file))
  (symbol-value variable))

(defun stash-clear (variable &optional with-file)
  (set variable nil)
  (when with-file
    (delete-file (get variable :file))))

(provide 'stash)
;;; stash.el ends here
