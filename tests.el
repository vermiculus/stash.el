(require 'ert)
(require 'ert-x)
(require 'stash)

(defmacro with-clean-cache (&rest body)
  `(let ((stash-directory (expand-file-name "./.test-caches")))
     (when (file-exists-p stash-directory)
       (delete-directory stash-directory t))
     (mkdir stash-directory)
     ,@body))

(ert-deftest simple ()
  (with-clean-cache
   (should
    (equal 'my-default
           (stash-new 'tmp "tmp.el" 'my-default)))

   (should
    (file-exists-p (stash-file 'tmp)))

   (should
    (equal 123
           (stash-set 'tmp 123)))

   (should
    (equal 123
           (stash-get 'tmp)))

   (should
    (equal 'my-default
           (stash-reset 'tmp)))

   (should
    (equal 'my-default
           (stash-get 'tmp)))))

(ert-deftest simple-with-idle-timer ()
  (with-clean-cache
   (should
    (equal 'my-default
           (stash-new 'tmp "tmp.el" 'my-default 1)))

   (should-not
    (file-exists-p (stash-file 'tmp)))

   (ert-run-idle-timers)

   (should
    (file-exists-p (stash-file 'tmp)))))
