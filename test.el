(require 'cachey "cachey.el")

(defmacro test-from-scratch (&rest body)
  `(let ((cachey-directory (expand-file-name "./.test-caches")))
     (ignore-errors (delete-directory cachey-directory t))
     ,@body))

(ert-deftest simple ()
  "simple test"
  (test-from-scratch
   (should
    (equal nil
           (progn
             (cachey-defstore 'tmp:store "store.el")
             (cachey-defcache 'tmp:store 'tmp:cache)
             (cachey-get 'tmp:cache))))))

(ert-deftest simple-with-sample-value ()
  "simple test"
  (test-from-scratch
   (should
    (equal 'sample-value
           (progn
             (cachey-defstore 'tmp:store "store.el")
             (cachey-defcache 'tmp:store 'tmp:cache nil 'sample-value)
             (cachey-get 'tmp:cache))))))

(ert-deftest simple-with-timeout ()
  "simple test"
  (test-from-scratch
   (should
    (equal nil
           (progn
             (cachey-defstore 'tmp:store "store.el")
             (cachey-defcache 'tmp:store 'tmp:cache 2)
             (cachey-get 'tmp:cache))))
   (sleep-for 1)
   (should
    (equal nil (cachey-get 'tmp:cache)))
   (sleep-for 1)
   (should-error
    (condition-case error-case
        (cachey-get 'tmp:cache)
      (cachey-cache-expired-error (error "Cache expired (as expected)"))))))

(ert-deftest simple-with-async ()
  "simple test"
  :expected-result :failed
  ;; Run this test by hand
  (test-from-scratch
   (cachey-defstore 'tmp:store "store.el" 1)
   (cachey-defcache 'tmp:store 'tmp:cache)
   (let ((dir (cachey--store-get-file 'tmp:store)))
     (should-not (file-exists-p dir))
     (cachey-put 'tmp:cache t)
     (should-not (file-exists-p dir))
     (sleep-for 4)
     (should (file-exists-p dir)))))
