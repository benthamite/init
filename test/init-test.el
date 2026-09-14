;;; init-test.el --- Tests for init -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ert)
(require 'init)

(ert-deftest init-build-profile-skips-unconfigured-user-config ()
  (let ((init-user-config-file nil)
	(build-steps nil))
    (cl-letf (((symbol-function 'init-set-babel-paths)
	       (lambda (_init-dir) (push 'paths build-steps)))
	      ((symbol-function 'init-load-excluded-packages-file)
	       (lambda (_init-dir) (push 'excluded-packages build-steps)))
	      ((symbol-function 'init-tangle-main-config-file)
	       (lambda () (push 'main-config build-steps)))
	      ((symbol-function 'run-hooks)
	       (lambda (&rest _hooks) (push 'post-build-hook build-steps))))
      (init-build-profile "/tmp/profile")
      (should (equal (nreverse build-steps)
		     '(paths excluded-packages main-config post-build-hook))))))

(ert-deftest init-load-excluded-packages-file-excludes-nothing-without-user-config ()
  (let ((init-user-config-file nil)
	(init-excluded-packages '(stale-package))
	(init-dir (make-temp-file "init-profile-" t)))
    (unwind-protect
	(progn
	  (init-load-excluded-packages-file init-dir)
	  (should (null init-excluded-packages)))
      (delete-directory init-dir t))))

(ert-deftest init-load-excluded-packages-file-errors-when-user-config-set ()
  (let ((init-user-config-file "/nonexistent/user-config.org")
	(init-dir (make-temp-file "init-profile-" t)))
    (unwind-protect
	(should-error (init-load-excluded-packages-file init-dir) :type 'user-error)
      (delete-directory init-dir t))))

(ert-deftest init-load-excluded-packages-file-loads-present-file ()
  (let ((init-user-config-file nil)
	(init-excluded-packages nil)
	(init-dir (make-temp-file "init-profile-" t)))
    (unwind-protect
	(progn
	  (with-temp-file (file-name-concat init-dir "excluded-packages.el")
	    (insert "(setq init-excluded-packages '(foo))\n"))
	  (init-load-excluded-packages-file init-dir)
	  (should (equal init-excluded-packages '(foo))))
      (delete-directory init-dir t))))

(ert-deftest init-tangle-user-config-file-errors-for-missing-file ()
  (let ((init-user-config-file
	 (make-temp-name
	  (expand-file-name "init-missing-user-config-" temporary-file-directory))))
    (should-error (init-tangle-user-config-file) :type 'user-error)))

(provide 'init-test)

;;; init-test.el ends here
