(defvar tlon-init-file-paths
  (file-name-concat user-emacs-directory "paths.el")
  "Path to `paths.el'.")

(defvar tlon-init-file-paths-override
  (file-name-concat user-emacs-directory "paths-override.el")
  "Path to `paths-override.el'.")

(defvar tlon-init-file-code-override
  (file-name-concat user-emacs-directory "code-override.el")
  "Path to `code-override.el'.")

(defvar tlon-init-file-tangle-flags
  (file-name-concat user-emacs-directory "tangle-flags.el")
  "Path to `tangle-flags.el'.")

(defvar tlon-init-file-early-init nil
  "Path to `early-init.el'.")

(defvar tlon-init-file-user-init
  (file-name-concat user-emacs-directory "init-pablo.el")
  "Path to `init-pablo.el'.")

(defvar tlon-init-file-late-init
  (file-name-concat user-emacs-directory "late-init.el")
  "Path to `late-init-el'.")

(defun tlon-init-post-init ()
  ;; Continue the init process, loading the files defined above, in tlon-init-filenames
  (dolist (key '(:init-2 :init-3 :init-4 :init-5))
    (let ((fname (eval (alist-get key tlon-init-filenames nil))))
      (if fname (load fname)))))

(load (file-name-concat user-emacs-directory "elpaca/repos/tlon-init/tlon-init.el")) ;; helper functions package
(tlon-init-set-paths)
(tlon-init-load-code-overrides)
(tlon-init-set-tangle-flags user-emacs-directory)
(load (eval tlon-init-file-user-init))
(add-hook 'elpaca-after-init-hook #'tlon-init-post-init)
