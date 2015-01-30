(require 'ert)
(require 'ert-x)

(require 'stash "stash.el")

(defmacro with-clean-cache (&rest body)
  `(let ((default-directory (expand-file-name "./.test-caches")))
     (when (file-exists-p default-directory)
       (delete-directory default-directory t))
     (mkdir default-directory)
     ,@body))

(ert-deftest simple ()
  (with-clean-cache
   (should
    (equal 'my-default
           (stash-new 'tmp "tmp.el" 'my-default)))
   (should
    (file-exists-p (get 'tmp :file)))
   (should
    (equal 123
           (stash-set 'tmp 123)))
   (should
    (equal 123
           (stash-get 'tmp)))
   (stash-reset 'tmp)
   (should
    (equal 'my-default
           (stash-get 'tmp)))))
