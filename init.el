;;; init.el --- Convenience functions to manage config profiles -*- lexical-binding: t -*-

;; Author: Federico Stafforini & Pablo Stafforini
;; Version: 1.3
;; Homepage: https://github.com/benthamite/init
;; Keywords: convenience tools
;; Package-Requires: ((paths "0.1") (elpaca "0.0.2"))

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
(require 'elpaca)
(require 'ob-tangle)
(require 'paths)
(require 'transient)

;;;; User options

(defgroup init ()
  "Convenience functions to manage Tlön's Emacs config."
  :group 'emacs)

(defcustom init-post-init-hook nil
  "Hook run at the end of the user’s config file.
If the user is Pablo, it is run at the end of `config.org'. Otherwise, it is run
at the end the user’s personal config file, e.g. `config-leonardo.org' if the
user is Leo.

The advantage of this hook over `elpaca-after-init-hook' is that the latter will
always load at the end of `config.org', even when the user is not Pablo."
  :type 'hook
  :group 'init)

(defcustom init-post-build-hook nil
  "Hook run after building a profile."
  :type 'hook
  :group 'init)

(defcustom init-post-deploy-hook nil
  "Hook run after deploying a profile."
  :type 'hook
  :group 'init)

(defcustom init-profiles-directory (expand-file-name "~/.config/emacs-profiles/")
  "Directory containing the Emacs profiles."
  :type 'directory
  :group 'init)

;;;; Variables

(defconst init-user-config-file (getenv "USER_CONFIG_FILE")
  "File with the user-specific configuration.
This is an `org-mode' file that contains the code blocks to be tangled into the
various user-specific Elisp files. See the `example.org' file for an example of
how this file should be organized.

The path to this file is specified via an environmental variable,
`USER_CONFIG_FILE', in your `.bashrc' or `.zshrc' file:

export USER_CONFIG_FILE=\"path/to/file.org\"")

;;;;; Files

(defvar init-file-early-init
  (file-name-concat user-emacs-directory "early-init.el")
  "Path to `early-init.el'.")

(defvar init-file-user-init
  (file-name-concat user-emacs-directory "init.el")
  "Path to `init.el'.")

(defvar init-file-late-init
  (file-name-concat user-emacs-directory "late-init.el")
  "Path to `late-init.el'.")

(defvar init-file-paths-override
  (file-name-concat user-emacs-directory "paths-override.el")
  "Path to `paths-override.el'.")

(defvar init-file-code-override
  (file-name-concat user-emacs-directory "code-override.el")
  "Path to `code-override.el'.")

(defvar init-file-excluded-packages
  (file-name-concat user-emacs-directory "excluded-packages.el")
  "Path to `excluded-packages.el'.")

;;;;; Other

(defvar init-excluded-packages '()
  "List of packages to be excluded in the tangle process.")

(defvar init-code-overrides '()
  "Alist of code overrides for each package.")

(defvar init-boot-as-if-not-pablo nil
  "If non-nil, boot as if the current machine is not Pablo’s.
This variable allows Pablo to test other people’s configs from his own computer.
It should be set in `init.el'.")

(defvar init-current-profile
  (file-name-nondirectory (directory-file-name user-emacs-directory))
  "Name of the current profile being used.")

(defconst init-system-name-pablo
  "Pablos-MacBook-Pro.local"
  "System name of Pablo's computer.")

;;;;; Lockfile

(defconst init-lockfile-name "lockfile.el"
  "Name of the lockfile.")

(defconst init-master-lockfile-path
  (file-name-concat paths-dir-dotemacs init-lockfile-name)
  "Path to the master lockfile to be propagated to the relevant profile dirs.")

;;;; Functions

;;;;; Functions used in code blocks

(declare-function org-get-heading "org")
;; TODO: consider removing the first argument
(defun init-tangle-conditionally (&optional package tangle-to-early-init bisect)
  "Tangle PACKAGE unless listed in `init-excluded-packages'.
By default, tangle to `init.el'. If TANGLE-TO-EARLY-INIT is non-nil, tangle to
`early-init.el' instead.

BISECT is a reserved argument for a functionality that has not yet been
developed."
  (let ((package (or package (intern (org-get-heading t t t t)))))
    (if bisect
	(init-process-for-bisection package)
      (init-get-tangle-target package tangle-to-early-init))))

(defun init-get-tangle-target (package early-init)
  "Return the file to which code block for PACKAGE should be tangled.
If EARLY-INIT is non-nil, return the early init file; else, return the main init
file."
  (let ((all-excluded (append init-excluded-packages
			      (mapcar (lambda (package)
					(intern (concat (symbol-name package) "-extras")))
				      init-excluded-packages))))
    ;; TODO: convert logic to `cond'
    (if (member package all-excluded)
	"no"
      (if early-init
	  init-file-early-init
	init-file-user-init))))

(defun init-override-code (key code-block)
  "Return CODE-BLOCK of KEY in `init-code-overrides'.
When KEY is not present in `init-code-overrides', return the default,
non-overridden code. The variable `init-code-overrides' is populated during
the init process.

The syntax for the KEY parameter is `:{package-name}' where `{package-name}' is
the name of the package. Examples: `:general', `:embark', `:hydra'.

Example usage:

\(init-override-code
 :embark
 \\='((use-package embark
       ;; Default, non-overridden code goes here,
       ;; in this case, the full use-package call
       )))

If `:embark' is found within `init-code-overrides' in this example, the
default will be overridden by that code."
  (with-temp-buffer
    (dolist (row (alist-get key init-code-overrides code-block))
      (insert (prin1-to-string row)))
    (eval-buffer)))

;;;;;

(defun init-read-file (fname)
  "Read FNAME and return its contents."
  (when fname
    (with-temp-buffer
      (insert-file-contents fname)
      (goto-char (point-min))
      (condition-case err
	  (read (current-buffer))
	(error
	 (error "Failed to parse %s: %s" fname (error-message-string err)))))))

(defun init-available-init-dirs ()
  "Return alist of profile names and their directories."
  (mapcar (lambda (name)
            (cons name (init-profile-dir name)))
          (init-list-profiles)))

(defun init-load-excluded-packages-file (init-dir)
  "Load the excluded packages list for INIT-DIR."
  (if (file-regular-p init-file-excluded-packages)
      (load-file init-file-excluded-packages)
    (user-error "`excluded-packages.el' not present in init directory `%s'" init-dir))
  (message "init: Loaded excluded packages for Emacs profile `%s'." init-current-profile))

(defun init-build-profile (init-dir)
  "Build or rebuild a profile in INIT-DIR."
  (interactive (list (init-profile-dir
		      (completing-read
		       "Select Emacs profile to build: "
		       (init-available-init-dirs)
		       nil t))))
  (init-set-babel-paths init-dir)
  ;; conditionally tangle extra config file, pass 1: get excluded packages only
  (init-tangle-user-config-file)
  (init-load-excluded-packages-file init-dir)
  (init-tangle-main-config-file)
  ;; conditionally tangle extra config file, pass 2: get the rest of extra config
  (init-tangle-user-config-file)
  (run-hooks 'init-post-build-hook))

;;;;; org-babel

(defun init-set-babel-paths (init-dir)
  "Set the paths for the `org-babel' code blocks relative to INIT-DIR."
  (setq init-file-paths-override (file-name-concat init-dir "paths-override.el")
	init-file-code-override (file-name-concat init-dir "code-override.el")
	init-file-excluded-packages (file-name-concat init-dir "excluded-packages.el")
	init-file-early-init (file-name-concat init-dir "early-init.el")
	init-file-user-init (file-name-concat init-dir "init.el")
	init-file-late-init (file-name-concat init-dir "late-init.el")))

(defun init-tangle ()
  "Tangle the current buffer."
  (widen)
  (save-buffer)
  (org-babel-tangle)
  (message "init: Tangled init files to Emacs profile `%s'." init-file-user-init))

(defun init-tangle-main-config-file ()
  "Tangle the main config file."
  (with-current-buffer (or (find-file-noselect paths-file-config)
			   (find-buffer-visiting paths-file-config))
    (init-tangle)))

(defun init-tangle-user-config-file ()
  "Tangle the user config file.
See `init-user-config-file' for details."
  (if (file-exists-p init-user-config-file)
      (with-current-buffer (find-file-noselect init-user-config-file)
	(init-tangle))
    (user-error "Extra config file for user %s not found" user-full-name)))

;;;;; Startup

(defun init-startup ()
  "Start up Emacs with `init' config."
  (message "init: Running startup...")
  (init-load-paths)
  (init-load-code-overrides)
  (init-load-excluded-packages-file user-emacs-directory)
  (init-defer-load-late-init))

(defun init-load-code-overrides ()
  "Load or re-load code overrides and from the currently booted init profile."
  (setq init-code-overrides
	(init-read-file init-file-code-override))
  (message "init: Loaded code overrides for Emacs profile `%s'." init-current-profile))

(defun init-defer-load-late-init ()
  "Load `late-init.el' file."
  (add-hook 'elpaca-after-init-hook #'init-load-late-init)
  (message "init: Added `init-load-late-init' to `elpaca-after-init-hook'."))

(defun init-load-late-init ()
  "Load `late-init.el'."
  (load init-file-late-init)
  (message "init: Loaded `late-init.el' for Emacs profile `%s'." init-current-profile))

(defun init-load-paths ()
  "Set paths from the currently booted init profile."
  (interactive)
  (init-load-default-paths)
  (init-load-override-paths)
  (message "init: Loaded paths for Emacs profile `%s'." init-current-profile))

(defun init-load-default-paths ()
  "Set paths in `paths.el', overriding them with `paths-override.el’ if present."
  (dolist (row (init-get-variables-and-values 'paths))
    (set (car row)
	 (init-eval-value-when-possible
	  (alist-get (car row) (init-read-file init-file-paths-override) (cdr row))))
    (message "init: Set `%s' to `%s'." (car row) (symbol-value (car row)))))

(defun init-load-override-paths ()
  "Set paths in `paths-override.el' not present in `paths.el'."
  (dolist (row (init-read-file init-file-paths-override))
    (unless (boundp (car row))
      (set (car row)
	   (init-eval-value-when-possible (cdr row)))
      (message "init: Set `%s' to `%s'." (car row) (symbol-value (car row))))))

(defun init-get-variables-and-values (group)
  "Return a list of lists of all variables and corresponding values in GROUP."
  (let (result)
    (dolist (member (custom-group-members group nil))
      (when (eq (cadr member) 'custom-variable)
	(let ((option (car member)))
	  (push `(,option . ,(symbol-value option)) result))))
    (nreverse result)))

(defun init-eval-value-when-possible (value)
  "Evaluate variable VALUE when possible, else return unevaluated VALUE."
  (condition-case _
      (eval value)
    (error value)))

(defun init-profile-dir (profile-name)
  "Return the directory of profile PROFILE-NAME."
  (file-name-concat init-profiles-directory profile-name))

;;;;; Profile management

(defun init-create-profile (profile-name &optional overwrite)
  "Create a new profile named PROFILE-NAME.
If profile already exists, throw error unless OVERWRITE is non-nil."
  (let ((profile-dir (file-name-concat init-profiles-directory profile-name)))
    (when (string-match file-name-invalid-regexp profile-name)
      (user-error "Invalid profile name"))
    (when (and (file-exists-p profile-dir) (not overwrite))
      (user-error "Profile already exists"))
    (make-directory profile-dir t)
    (message "Created new profile '%s'" profile-name)
    profile-dir))

(defun init-delete-profile (profile-name &optional skip-confirmation)
  "Delete profile with name PROFILE-NAME.
If SKIP-CONFIRMATION is non-nil, skip confirmation prompt."
  (interactive
   (list (completing-read "Profile to delete: "
                          (init-list-profiles))))
  (let ((profile-dir (file-name-concat init-profiles-directory profile-name)))
    (when (and (file-exists-p profile-dir)
               (or skip-confirmation
		   (yes-or-no-p (format "Really delete profile '%s'? " profile-name))))
      (delete-directory profile-dir t)
      (message "Deleted profile '%s'" profile-name))))

(defun init-maybe-delete-profile (profile-name)
  "Prompt to delete PROFILE-NAME if it already exists."
  (when (init-profile-exists-p profile-name)
    (if (y-or-n-p (format "Profile %s already exists. Delete existing profile and redeploy? " profile-name))
	(init-delete-profile profile-name 'skip-confirmation)
      (user-error "Aborted"))))

(defun init-list-profiles ()
  "Return a list of available profile names."
  (when (file-exists-p init-profiles-directory)
    (seq-filter (lambda (name)
                  (and (not (string-prefix-p "." name))
                       (file-directory-p (file-name-concat init-profiles-directory name))))
                (directory-files init-profiles-directory))))

(defun init-get-tag ()
  "Get the tag of local `dotfiles' repository."
  (let ((default-directory paths-dir-dotemacs))
    (string-trim (shell-command-to-string "git describe --tags --abbrev=0"))))

(defvar elpaca-lock-file)
(declare-function elpaca-write-lock-file "elpaca")
(defun init-deploy-profile (&optional profile-name)
  "Deploy PROFILE-NAME."
  (interactive)
  (let ((default-directory paths-dir-dotemacs))
    (if (zerop (magit-git-exit-code "pull"))
	(let ((profile-name (or profile-name (read-string "Profile name: " (init-get-tag)))))
	  (init-maybe-delete-profile profile-name)
	  (init-create-profile profile-name t)
	  (init-maybe-write-lockfile)
	  (init-copy-lockfile (init-profile-dir profile-name))
	  (if (and (boundp 'paths-file-config)
		   (y-or-n-p " Build init files?"))
	      (with-current-buffer (or (find-file-noselect paths-file-config)
				       (find-buffer-visiting paths-file-config))
		(init-build-profile (init-profile-dir profile-name)))
	    (run-hooks 'init-post-deploy-hook)
	    (message (format "Deployed profile '%s'." profile-name))))
      (user-error "Pull from dotfiles failed. Please check repository status"))))

(defun init-profile-exists-p (profile-name)
  "Return non-nil if profile PROFILE-NAME exists."
  (file-directory-p (file-name-concat init-profiles-directory profile-name)))

;;;;; lock-file

(defun init-get-lockfile (&optional profile-name)
  "Return the path to the lockfile for PROFILE-NAME.
If PROFILE-NAME is nil, return the lockfile for the current profile."
  (let ((elpaca-dir (if profile-name
			(file-name-concat (init-profile-dir profile-name) "elpaca/")
		      elpaca-directory)))
    (file-name-concat elpaca-dir init-lockfile-name)))

(defun init-copy-lockfile (dest-dir)
  "Copy `init-lockfile-name' from dotfiles directory to DEST-DIR.
If the source lockfile is missing, do nothing."
  (let* ((src (file-name-concat (file-name-directory paths-file-config) init-lockfile-name))
	 (dest (file-name-concat dest-dir init-lockfile-name)))
    (when (file-exists-p src)
      (copy-file src dest t)
      (message "init: Copied `%s' to `%s'." init-lockfile-name dest))))

(defun init-maybe-write-lockfile ()
  "Prompt to write the lockfile if system name equals `init-system-name-pablo'."
  (when (and (string= (system-name) init-system-name-pablo)
	     (y-or-n-p "Write lockfile? "))
    (elpaca-write-lock-file init-master-lockfile-path)))

;;;;; Update package

(declare-function elpaca-extras-update-and-reload "elpaca-extras")
(defun init-update-and-reload ()
  "Update and reload the `init' package."
  (interactive)
  (elpaca-extras-update-and-reload 'init))

;;;;; Update conifg

(autoload 'magit-git-exit-code "magit-git")
(defun init-update-config ()
  "Update the user-specific configuration."
  (interactive)
  (let* ((default-directory paths-dir-dotemacs))
    (if (zerop (magit-git-exit-code "pull"))
        (when (y-or-n-p "Config updated. Deploy new profile? ")
          (init-deploy-profile))
      (user-error "Pull failed. Please check repository status"))))

;;;;; Bisection

(defun init-process-for-bisection (package)
  "<explain behavior> PACKAGE."
  package
  (user-error "This function is not yet defined"))

;;;;; Menu

;;;###autoload (autoload 'init-menu "init" nil t)
(transient-define-prefix init-menu ()
  "`init' menu."
  [["Profile"
    ("b" "build"                           init-build-profile)
    ("d" "deploy"                          init-deploy-profile)
    ("x" "delete"                          init-delete-profile)]
   ["Package"
    ("H-u" "update & reload"               init-update-and-reload)]])

(provide 'init)

;;; init.el ends here
