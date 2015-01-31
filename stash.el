;;; stash.el --- lightweight persistent caching

;; Copyright (C) 2015  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; URL: https://www.github.com/vermiculus/stash.el/
;; Version: 0.2
;; Keywords: extensions, data, internal, lisp
;; Package-Requires: ((cl-lib "0.5"))

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

;; The basic unit of stash.el is the app.  Apps define groups of
;; related variables.  At an interval defined by each application, its
;; variables are written to disk.  Where no app is given, the `nil'
;; app is set to save every minute.

;;; Code:
(eval-when-compile
  (require 'cl-lib))


;;; User Customizations

(defgroup stash nil
  "Customization group for stash."
  :prefix "stash-"
  :group 'emacs)

(defcustom stash-directory
  (file-name-as-directory
   (locate-user-emacs-file "stashes"))
  "Directory where stash variable files are saved by default."
  :type 'directory
  :group 'stash)


;;; Internal Variables

(defvar stash-app-list nil
  "\(\(APP STASH...))")


;;; Convenience Functions

(defsubst stash-app-members (app)
  (cdr (stash-app app)))

(defsubst stash-app-timer (app)
  (get app 'stash-timer))

(defsubst stash-app-write-delay (app)
  (get app 'stash-write-delay))

(defsubst stash-app (app)
  (assq app stash-app-list))

(defsubst stash-file (variable)
  "Return VARIABLE's associated file.
The filename is expanded within the context of
`stash-directory'."
  (expand-file-name
   (get variable 'stash-file)
   stash-directory))

(defsubst stash-default-value (variable)
  (get variable 'stash-default-value))

(defsubst stash-app-members (app)
  (cdr (stash-app app)))

(defsubst stash-owning-app (variable)
  (get variable 'stash-app))


;;; Saving and Loading Variables

(defun stash-read (file default)
  "Return the data in FILE.
If FILE is not readable, return DEFAULT.

Note: FILE is expected to contain the data structure as a single
symbolic expression (sexp).  If there are many sexps in FILE,
this function will only return the first.  This is of no concern
if FILE was written by `stash-save'."
  (if (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (read (current-buffer)))
    default))

(defun stash-load (variable)
  "Read and set VARIABLE from disk.
If the associated file does not exist, the value of VARIABLE is
reset."
  (set
   variable
   (stash-read
    (stash-file variable)
    (stash-default-value variable))))

(defun stash-save (variable)
  "Write VARIABLE's data to disk."
  (let* ((cachefile (stash-file variable))
         (dir (file-name-directory cachefile)))
    (unless (file-exists-p dir)
      (make-directory dir))
    (write-region
     (let (print-length print-level)
       (prin1-to-string (symbol-value variable)))
     nil
     (stash-file variable)))
  variable)

(defun stash-set-and-save (variable value)
  "Set VARIABLE to VALUE and write its group to disk immediately.
Return VALUE."
  (set variable value)
  (stash-app-save (stash-owning-app variable))
  value)

(defun stash-app-save (app)
  (mapc #'stash-save (cdr (assq app stash-app-list))))

(defun stash-reset (variable)
  "Reset VARIABLE to its initial value."
  (set variable (stash-default-value variable)))


;;; Defining Applications and Stashes

;;;###autoload
(defalias 'defstash #'stash-new)

;;;###autoload
(defalias 'defapp #'stash-app-new)

;;;###autoload
(defmacro stash-new (variable file &optional app default-value docstring)
  "Define VARIABLE as a new stash to be written to FILE.
VARIABLE's default value will be DEFAULT-VALUE.  When set, it
will automatically be written to disk after Emacs is idle for
WRITE-DELAY seconds."
  (declare (indent 4) (doc-string 5))
  (let ((g (assq app stash-app-list)))
    (if (or g (null app))
        (when (not (memq variable (cdr g)))
          (setcdr g (cons variable (cdr g))))
      (error "Stash application `%S' is not defined" app)))
  `(prog1 (defvar ,variable ,default-value ,docstring)
    (put ',variable 'stash-file ,file)
    (put ',variable 'stash-default-value ,default-value)
    (put ',variable 'stash-app ',app)
    (set ',variable ,default-value)))

;;;###autoload
(defmacro stash-app-new (app write-delay)
  (declare (debug (name body)))
  (let ((app (if (null app) nil `',app)))
    `(prog1 ,app
       (let ((this-app (assq ,app stash-app-list)))
         (unless this-app
           (let ((app-spec (list ,app)))
             (add-to-list 'stash-app-list app-spec)
             (setq this-app app-spec)))
         (let ((app (car this-app)))
           (put app 'stash-write-delay ,write-delay)
           (put app 'stash-subdirectory (symbol-name ,app))
           (stash-app-timer-reset ,app))))))


;;; Timers

(defun stash-app-timer-cancel (app)
  (let ((timer (stash-app-timer app)))
    ;; star trek tgn s4e22 "half a life" :(
    (when timer
      (cancel-timer timer)))
  app)

(defun stash-app-timer-reset (app)
  (stash-app-timer-cancel app)
  (put app 'stash-timer
       (run-with-idle-timer
        (stash-app-write-delay app)
        t #'stash-app-save app))
  app)

;;;###autoload
(defun stash-timer-cancel-all ()
  (interactive)
  (mapcar
   #'stash-app-timer-cancel
   (mapcar #'car stash-app-list)))

;;;###autoload
(defun stash-timer-reset-all ()
  (interactive)
  (mapcar
   #'stash-app-timer-reset
   (mapcar #'car stash-app-list)))


;;; Initialization

;; If no application is given, save the stash every minute
(stash-app-new nil 60)

(provide 'stash)
;;; stash.el ends here
