(defvar ps/init-pafilenames
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
`ps/init-override-code-if-present', which is created by this init
process. The convention for key names is :ps/{package-name} but
it is not required by this code.

`:init-N': (N between 1 and 5) the files containing the actual
init code. They will be loaded in the order set by their keyname,
not by their position in `ps/init-filenames'.
")

(defun ps/init-post-init ()
;; Continue the init process, loading the files defined above, in ps/init-filenames
(dolist (key '(:init-2 :init-3 :init-4 :init-5))
  (let ((fname (eval (alist-get key ps/init-filenames nil))))
    (if fname (load fname))
    )))


(load (file-name-concat user-emacs-directory "tlon-init.el")) ;; helper functions package
(ps/init-load-variables)
(ps/init-load-code-overrides)
(ps/init-set-tangle-options user-emacs-directory)
(load (eval (alist-get :init-1 ps/init-filenames)))
(add-hook 'elpaca-after-init-hook #'ps/init-post-init)
