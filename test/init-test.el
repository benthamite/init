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

(ert-deftest init-tangle-user-config-file-errors-for-missing-file ()
  (let ((init-user-config-file
	 (make-temp-name
	  (expand-file-name "init-missing-user-config-" temporary-file-directory))))
    (should-error (init-tangle-user-config-file) :type 'user-error)))

(provide 'init-test)

;;; init-test.el ends here
