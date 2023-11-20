(defvar tlon-init-filenames
  '(
    (:variables-default . (file-name-concat user-emacs-directory "variables.el"))
    (:variables-override . (file-name-concat user-emacs-directory "variables-override.el"))
    (:code-overrides . (file-name-concat user-emacs-directory "code-overrides.el"))
    (:init-1 . (file-name-concat user-emacs-directory "init-pablo.el"))
    (:init-2 . (file-name-concat user-emacs-directory "post-init.el"))
    )
  "An alist with the configuration filenames used in the
init process:

`:variables-default': a file from which default variable values
will read and set.

`:variables-override': (optional) file from which to override
default variable values.

`:code-overrides': (optional) file from which to override code
sections in the init file, using the function
`tlon-init-override-code-if-present', which is created by this init
process. The convention for key names is :ps/{package-name} but
it is not required by this code.

`:init-N': (N between 1 and 5) the files containing the actual
init code. They will be loaded in the order set by their keyname,
not by their position in `tlon-init-filenames'.
")

(defun tlon-init-post-init ()
  ;; Continue the init process, loading the files defined above, in tlon-init-filenames
  (dolist (key '(:init-2 :init-3 :init-4 :init-5))
    (let ((fname (eval (alist-get key tlon-init-filenames nil))))
      (if fname (load fname)))))

(load (file-name-concat user-emacs-directory "elpaca/repos/tlon-init/tlon-init.el")) ;; helper functions package
(tlon-init-set-paths)
(tlon-init-load-code-overrides)
(tlon-init-set-tangle-flags user-emacs-directory)
(load (eval (alist-get :init-1 tlon-init-filenames)))
(add-hook 'elpaca-after-init-hook #'tlon-init-post-init)
