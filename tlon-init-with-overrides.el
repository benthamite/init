(load (file-name-concat user-emacs-directory "elpaca/repos/tlon-init/tlon-init.el")) ;; helper functions package

(defun tlon-init-post-init ()
  "Continue the init process, loading the files defined above, in `tlon-init-filenames'."
  (dolist (key '(:init-2 :init-3 :init-4 :init-5))
    (let ((fname (eval (alist-get key tlon-init-filenames nil))))
      (if fname (load fname)))))

(tlon-init-set-paths)
(tlon-init-load-code-overrides)
(tlon-init-set-tangle-flags user-emacs-directory)
(load (eval tlon-init-file-user-init))
(add-hook 'elpaca-after-init-hook #'tlon-init-post-init)
