;;; tlon-init.el --- Convenience functions to manage Tlön's Emacs config -*- lexical-binding: t -*-

;; Author: Federico Stafforini & Pablo Stafforini
;; Version: 1.2
;; Homepage: https://github.com/tlon-team/tlon-init
;; Keywords: convenience tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Convenience functions to manage Tlön's Emacs config

;;; Code:

(require 'cus-edit)
(require 'ob-tangle)
(require 'paths)

;;;; User options

(defgroup tlon-init ()
  "Convenience functions to manage Tlön's Emacs config."
  :group 'emacs)

(defcustom tlon-init-user-config-file (getenv "USER_CONFIG_FILE")
  "File with the user-specific configuration.
This is an `org-mode' file that contains the code blocks to be tangled into the
various user-specific Elisp files. See the `example.org' file for an example of
how this file should be organized.

The path to this file is specified via an environmental variable,
`USER_CONFIG_FILE', in your `.bashrc' or `.zshrc' file:

export USER_CONFIG_FILE=\"path/to/file.org\""
  :type 'file
  :group 'tlon-init)

(defcustom tlon-init-post-init-hook nil
  "Hook run at the end of the user’s config file.
If the user is Pablo, it is run at the end of `config.org'. Otherwise, it is run
at the end the user’s personal config file, e.g. `config-leonardo.org' if the
user is Leo.

The advantage of this hook over `elpaca-after-init-hook' is that the latter will
always load at the end of `config.org', even when the user is not Pablo."
  :type 'hook
  :group 'tlon-init)

(defcustom tlon-init-post-build-hook nil
  "Hook run after building a profile."
  :type 'hook
  :group 'tlon-init)

(defcustom tlon-init-post-deploy-hook nil
  "Hook run after deploying a profile."
  :type 'hook
  :group 'tlon-init)

(defcustom tlon-init-profiles-directory (expand-file-name "~/.config/emacs-profiles/")
  "Directory containing the Emacs profiles."
  :type 'directory
  :group 'tlon-init)

;;;; Variables

;;;;; Files

(defvar tlon-init-file-early-init
  (file-name-concat user-emacs-directory "early-init.el")
  "Path to `early-init.el'.")

(defvar tlon-init-file-user-init
  (file-name-concat user-emacs-directory "init.el")
  "Path to `init.el'.")

(defvar tlon-init-file-late-init
  (file-name-concat user-emacs-directory "late-init.el")
  "Path to `late-init.el'.")

(defvar tlon-init-file-paths-override
  (file-name-concat user-emacs-directory "paths-override.el")
  "Path to `paths-override.el'.")

(defvar tlon-init-file-code-override
  (file-name-concat user-emacs-directory "code-override.el")
  "Path to `code-override.el'.")

(defvar tlon-init-file-excluded-packages
  (file-name-concat user-emacs-directory "excluded-packages.el")
  "Path to `excluded-packages.el'.")

;;;;; Other

(defvar tlon-init-excluded-packages '()
  "List of packages to be excluded in the tangle process.")

(defvar tlon-init-code-overrides '()
  "Alist of code overrides for each package.")

(defvar tlon-init-boot-as-if-not-pablo nil
  "If non-nil, boot as if the current machine is not Pablo’s.
This variable allows Pablo to test other people’s configs from his own computer.
It should be set in `init.el'.")

(defvar tlon-init-current-profile
  (file-name-nondirectory (directory-file-name user-emacs-directory))
  "Name of the current profile being used.")

;;;; Functions

;;;;; Functions used in code blocks

(declare-function org-get-heading "org")
;; TODO: consider removing the first argument
(defun tlon-init-tangle-conditionally (&optional package tangle-to-early-init bisect)
  "Tangle PACKAGE unless listed in `tlon-init-excluded-packages'.
By default, tangle to `init.el'. If TANGLE-TO-EARLY-INIT is non-nil, tangle to
`early-init.el' instead.

BISECT is a reserved argument for a functionality that has not yet been
developed."
  (let ((package (or package (intern (org-get-heading t t t t)))))
    (if bisect
	(tlon-init-process-for-bisection package)
      (tlon-init-get-tangle-target package tangle-to-early-init))))

(defun tlon-init-get-tangle-target (package early-init)
  "Return the file to which code block for PACKAGE should be tangled.
If EARLY-INIT is non-nil, return the early init file; else, return the main init
file."
  (let ((all-excluded (append tlon-init-excluded-packages
			      (mapcar (lambda (package)
					(intern (concat (symbol-name package) "-extras")))
				      tlon-init-excluded-packages))))
    ;; TODO: convert logic to `cond'
    (if (member package all-excluded)
	"no"
      (if early-init
	  tlon-init-file-early-init
	tlon-init-file-user-init))))

(defun tlon-init-override-code (key code-block)
  "Return CODE-BLOCK of KEY in `tlon-init-code-overrides'.
When KEY is not present in `tlon-init-code-overrides', return the default,
non-overridden code. The variable `tlon-init-code-overrides' is populated during
the init process.

The syntax for the KEY parameter is `:{package-name}' where `{package-name}' is
the name of the package. Examples: `:general', `:embark', `:hydra'.

Example usage:

\(tlon-init-override-code
 :embark
 \\='((use-package embark
       ;; Default, non-overridden code goes here,
       ;; in this case, the full use-package call
       )))

If `:embark' is found within `tlon-init-code-overrides' in this example, the
default will be overridden by that code."
  (with-temp-buffer
    (dolist (row (alist-get key tlon-init-code-overrides code-block))
      (insert (prin1-to-string row)))
    (eval-buffer)))

;;;;;

(defun tlon-init-read-file (fname)
  "Read FNAME and return its contents."
  (when fname
    (with-temp-buffer
      (insert-file-contents fname)
      (goto-char (point-min))
      (condition-case err
	  (read (current-buffer))
	(error
	 (error "Failed to parse %s: %s" fname (error-message-string err)))))))

(defun tlon-init-available-init-dirs ()
  "Return alist of profile names and their directories."
  (mapcar (lambda (name)
            (cons name (tlon-init-profile-dir name)))
          (tlon-init-list-profiles)))

(defun tlon-init-machine-pablo-p ()
  "Return t if Pablo's machine is the current machine, and nil otherwise.
Set `tlon-init-boot-as-if-not-pablo' to t in `init.el' to test from Pablo's
machine"
  (and (string= (system-name) "Pablos-MacBook-Pro.local")
       (not tlon-init-boot-as-if-not-pablo)))

(defun tlon-init-load-excluded-packages-file (init-dir)
  "Load the excluded packages list for INIT-DIR."
  (unless (tlon-init-machine-pablo-p)
    (if (file-regular-p tlon-init-file-excluded-packages)
	(load-file tlon-init-file-excluded-packages)
      (user-error "`excluded-packages.el' not present in init directory `%s'" init-dir))
    (message "tlon-init: Loaded excluded packages for Emacs profile `%s'." tlon-init-current-profile)))

(defun tlon-init-build (init-dir)
  "Build or rebuild a profile in INIT-DIR."
  (interactive (list (tlon-init-profile-dir
		      (completing-read
		       "Select Emacs profile to build: "
		       (tlon-init-available-init-dirs)
		       nil t))))
  (tlon-init-set-babel-paths init-dir)
  ;; conditionally tangle extra config file, pass 1: get excluded packages only
  (tlon-init-tangle-user-config-file)
  (tlon-init-load-excluded-packages-file init-dir)
  (tlon-init-tangle-main-config-file)
  ;; conditionally tangle extra config file, pass 2: get the rest of extra config
  (tlon-init-tangle-user-config-file)
  (run-hooks 'tlon-init-post-build-hook))

;;;;; org-babel

(defun tlon-init-set-babel-paths (init-dir)
  "Set the paths for the `org-babel' code blocks relative to INIT-DIR."
  (setq tlon-init-file-paths-override (file-name-concat init-dir "paths-override.el")
	tlon-init-file-code-override (file-name-concat init-dir "code-override.el")
	tlon-init-file-excluded-packages (file-name-concat init-dir "excluded-packages.el")
	tlon-init-file-early-init (file-name-concat init-dir "early-init.el")
	tlon-init-file-user-init (file-name-concat init-dir "init.el")
	tlon-init-file-late-init (file-name-concat init-dir "late-init.el")))

(defun tlon-init-tangle ()
  "Tangle the current buffer."
  (widen)
  (save-buffer)
  (org-babel-tangle)
  (message "tlon-init: Tangled init files to Emacs profile `%s'." tlon-init-file-user-init))

(defun tlon-init-tangle-main-config-file ()
  "Tangle the main config file."
  (with-current-buffer (or (find-file-noselect paths-file-config)
			   (find-buffer-visiting paths-file-config))
    (tlon-init-tangle)))

(defun tlon-init-tangle-user-config-file ()
  "Tangle the user config file.
See `tlon-init-user-config-file' for details."
  (if (file-exists-p tlon-init-user-config-file)
      (with-current-buffer (find-file-noselect tlon-init-user-config-file)
	(tlon-init-tangle))
    (user-error "Extra config file for user %s not found" user-full-name)))

;;;;; Startup

(defun tlon-init-startup ()
  "Start up Emacs with `tlon-init' config."
  (message "tlon-init: Running startup...")
  (tlon-init-load-paths)
  (tlon-init-load-code-overrides)
  (tlon-init-load-excluded-packages-file user-emacs-directory)
  (tlon-init-defer-load-late-init))

(defun tlon-init-run-post-init-hook ()
  "Run `tlon-init-post-init-hook'."
  (when (tlon-init-machine-pablo-p)
    (dolist (hook tlon-init-post-init-hook)
      (let ((hook-name (if (symbolp hook)
			   (symbol-name hook)
			 "a lambda function")))
	(message "Running `%s'." hook-name)))
    (message "tlon-init: Running of hooks in `tlon-init-post-init-hook' complete")
    (run-hooks 'tlon-init-post-init-hook)))

(defun tlon-init-load-code-overrides ()
  "Load or re-load code overrides and from the currently booted init profile."
  (unless (tlon-init-machine-pablo-p)
    (setq tlon-init-code-overrides
	  (tlon-init-read-file tlon-init-file-code-override))
    (message "tlon-init: Loaded code overrides for Emacs profile `%s'." tlon-init-current-profile)))

(defun tlon-init-defer-load-late-init ()
  "Load `late-init.el' file."
  (unless (tlon-init-machine-pablo-p)
    (add-hook 'elpaca-after-init-hook #'tlon-init-load-late-init)
    (message "tlon-init: Added `tlon-init-load-late-init' to `elpaca-after-init-hook'.")))

(defun tlon-init-load-late-init ()
  "Load `late-init.el'."
  (load tlon-init-file-late-init)
  (message "tlon-init: Loaded `late-init.el' for Emacs profile `%s'." tlon-init-current-profile))

(defun tlon-init-load-paths ()
  "Set paths from the currently booted init profile."
  (interactive)
  (unless (tlon-init-machine-pablo-p)
    (tlon-init-load-default-paths)
    (tlon-init-load-override-paths)
    (message "tlon-init: Loaded paths for Emacs profile `%s'." tlon-init-current-profile)))

(defun tlon-init-load-default-paths ()
  "Set paths in `paths.el', overriding them with `paths-override.el’ if present."
  (dolist (row (tlon-init-get-variables-and-values 'paths))
    (set (car row)
	 (tlon-init-eval-value-when-possible
	  (alist-get (car row) (tlon-init-read-file tlon-init-file-paths-override) (cdr row))))
    (message "tlon-init: Set `%s' to `%s'." (car row) (symbol-value (car row)))))

(defun tlon-init-load-override-paths ()
  "Set paths in `paths-override.el' not present in `paths.el'."
  (dolist (row (tlon-init-read-file tlon-init-file-paths-override))
    (unless (boundp (car row))
      (set (car row)
	   (tlon-init-eval-value-when-possible (cdr row)))
      (message "tlon-init: Set `%s' to `%s'." (car row) (symbol-value (car row))))))

(defun tlon-init-get-variables-and-values (group)
  "Return a list of lists of all variables and corresponding values in GROUP."
  (let (result)
    (dolist (member (custom-group-members group nil))
      (when (eq (cadr member) 'custom-variable)
	(let ((option (car member)))
	  (push `(,option . ,(symbol-value option)) result))))
    (nreverse result)))

(defun tlon-init-eval-value-when-possible (value)
  "Evaluate variable VALUE when possible, else return unevaluated VALUE."
  (condition-case _
      (eval value)
    (error value)))

(defun tlon-init-profile-dir (profile-name)
  "Return the directory of profile PROFILE-NAME."
  (file-name-concat tlon-init-profiles-directory profile-name))

;;;;; Profile management

(defun tlon-init-create-profile (profile-name &optional overwrite)
  "Create a new profile named PROFILE-NAME.
If profile already exists, throw error unless OVERWRITE is non-nil."
  (let ((profile-dir (file-name-concat tlon-init-profiles-directory profile-name)))
    (when (string-match file-name-invalid-regexp profile-name)
      (user-error "Invalid profile name"))
    (when (and (file-exists-p profile-dir) (not overwrite))
      (user-error "Profile already exists"))
    (make-directory profile-dir t)
    (message "Created new profile '%s'" profile-name)
    profile-dir))

(defun tlon-init-delete-profile (profile-name)
  "Delete profile with name PROFILE-NAME."
  (interactive
   (list (completing-read "Profile to delete: "
                          (tlon-init-list-profiles))))
  (let ((profile-dir (file-name-concat tlon-init-profiles-directory profile-name)))
    (when (and (file-exists-p profile-dir)
               (yes-or-no-p (format "Really delete profile '%s'? " profile-name)))
      (delete-directory profile-dir t)
      (message "Deleted profile '%s'" profile-name))))

(defun tlon-init-list-profiles ()
  "Return a list of available profile names."
  (when (file-exists-p tlon-init-profiles-directory)
    (seq-filter (lambda (name)
                  (and (not (string-prefix-p "." name))
                       (file-directory-p (file-name-concat tlon-init-profiles-directory name))))
                (directory-files tlon-init-profiles-directory))))

(defun tlon-init-get-tag ()
  "Get the tag of local `dotfiles' repository."
  (let ((default-directory paths-dir-dotemacs))
    (string-trim (shell-command-to-string "git describe --tags --abbrev=0"))))

(defun tlon-init-deploy-profile (&optional profile-name)
  "Deploy PROFILE-NAME."
  (interactive)
  (let ((profile-name (or profile-name (read-string "Profile name: " (tlon-init-get-tag)))))
    (when (tlon-init-profile-exists-p profile-name)
      (unless (y-or-n-p (format "Profile `%s' already exists. Redeploy? " profile-name))
        (user-error "Aborted")))
    (tlon-init-create-profile profile-name t)
    (if (and (boundp 'paths-file-config)
             (y-or-n-p " Build init files?"))
        (with-current-buffer (or (find-file-noselect paths-file-config)
				 (find-buffer-visiting paths-file-config))
          (tlon-init-build (tlon-init-profile-dir profile-name)))
      (run-hooks 'tlon-init-post-deploy-hook)
      (message (format "Deployed profile '%s'." profile-name)))))

(defun tlon-init-profile-exists-p (profile-name)
  "Return non-nil if profile PROFILE-NAME exists."
  (file-directory-p (file-name-concat tlon-init-profiles-directory profile-name)))

;;;;; Bisection

(defun tlon-init-process-for-bisection (package)
  "<explain behavior> PACKAGE."
  package
  (user-error "This function is not yet defined"))

(provide 'tlon-init)

;;; tlon-init.el ends here
