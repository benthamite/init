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

(ert-deftest init-read-file-returns-nil-for-absent-file-without-user-config ()
  (let ((init-user-config-file nil)
	(init-dir (make-temp-file "init-profile-" t)))
    (unwind-protect
	(should (null (init-read-file (file-name-concat init-dir "paths-override.el"))))
      (delete-directory init-dir t))))

(ert-deftest init-read-file-errors-for-absent-file-when-user-config-set ()
  (let ((init-user-config-file "/nonexistent/user-config.org")
	(init-dir (make-temp-file "init-profile-" t)))
    (unwind-protect
	(should-error (init-read-file (file-name-concat init-dir "paths-override.el"))
		      :type 'user-error)
      (delete-directory init-dir t))))

(ert-deftest init-load-late-init-runs-hook-without-late-init-file ()
  (let* ((init-user-config-file nil)
	 (init-dir (make-temp-file "init-profile-" t))
	 (init-file-late-init (file-name-concat init-dir "late-init.el"))
	 (runs 0)
	 (init-post-init-hook (list (lambda () (setq runs (1+ runs))))))
    (unwind-protect
	(progn
	  (init-load-late-init)
	  (should (= runs 1)))
      (delete-directory init-dir t))))

(defvar init-test-late-init-events nil
  "Events recorded while loading a test `late-init.el'.")

(ert-deftest init-load-late-init-loads-file-then-runs-hook-once ()
  (let* ((init-user-config-file nil)
	 (init-dir (make-temp-file "init-profile-" t))
	 (init-file-late-init (file-name-concat init-dir "late-init.el"))
	 (init-test-late-init-events nil)
	 (init-post-init-hook
	  (list (lambda () (push 'hook init-test-late-init-events)))))
    (unwind-protect
	(progn
	  (with-temp-file init-file-late-init
	    (insert "(push 'late-init init-test-late-init-events)\n"))
	  (init-load-late-init)
	  (should (equal (reverse init-test-late-init-events) '(late-init hook))))
      (delete-directory init-dir t))))

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

(ert-deftest init-pablo-system-p-accepts-bonjour-suffix ()
  (dolist (case '(("Pablos-MacBook-Pro.local" . t)
		  ("Pablos-MacBook-Pro-2.local" . t)
		  ("Pablos-MacBook-Pro-13.local" . t)
		  ("Pablos-MacBook-Pro" . nil)
		  ("Leos-MacBook-Pro.local" . nil)
		  ("XPablos-MacBook-Pro.local" . nil)))
    (cl-letf (((symbol-function 'system-name) (lambda () (car case))))
      (should (eq (init-pablo-system-p) (cdr case))))))

(ert-deftest init-commit-and-push-lockfile-tolerates-refused-push ()
  (let ((paths-dir-dotemacs temporary-file-directory)
	(init-master-lockfile-path "/tmp/lockfile.el")
	(git-calls nil))
    (cl-letf (((symbol-function 'shell-command-to-string) (lambda (&rest _) " M lockfile.el\n"))
	      ((symbol-function 'magit-git-exit-code)
	       (lambda (&rest args)
		 (push (car args) git-calls)
		 (if (equal (car args) "push") 1 0))))
      (init-commit-and-push-lockfile)
      (should (equal (nreverse git-calls) '("add" "commit" "push"))))))

(ert-deftest init-commit-and-push-lockfile-errors-when-commit-fails ()
  (let ((paths-dir-dotemacs temporary-file-directory)
	(init-master-lockfile-path "/tmp/lockfile.el"))
    (cl-letf (((symbol-function 'shell-command-to-string) (lambda (&rest _) " M lockfile.el\n"))
	      ((symbol-function 'magit-git-exit-code)
	       (lambda (&rest args) (if (equal (car args) "commit") 1 0))))
      (should-error (init-commit-and-push-lockfile) :type 'user-error))))

(ert-deftest init-tangle-user-config-file-errors-for-missing-file ()
  (let ((init-user-config-file
	 (make-temp-name
	  (expand-file-name "init-missing-user-config-" temporary-file-directory))))
    (should-error (init-tangle-user-config-file) :type 'user-error)))

(provide 'init-test)

;;; init-test.el ends here
