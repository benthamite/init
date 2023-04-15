;;; tlon-init.el --- Convenience functions to manage Tlön's Emacs config -*- lexical-binding: t -*-

;; Author: Federico Stafforini
;; Version: 0.1.0
;; Homepage: https://tlon.team
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

;;; Code:

(defun tlon-init-get-tangle-flag (key &optional tangle-to-early-init)
  "Get a `yes' / `no' tangle flag for a given KEY.
Use the default config as a base, which is overridden by values
in the active config when present.

If no key is present returns `yes', so that the default behavior
is to tangle blocks that are not present in `tlon-init-flags'.

The syntax for the KEY parameter is `:ps/{package-name}' where
`{package-name}' is the name of the package. Examples:
`:ps/general', `:ps/embark', `:ps/hydra'.

With optional TANGLE-TO-EARLY-INIT, tangle to the `early-init.el'
file."
  (if (alist-get key tlon-init-tangle-flags t)
      (if tangle-to-early-init
	  tlon-init-early-init-path
	tlon-init-user-init-path)
    "no"))

(defun tlon-init-override-code-if-available (key code-block)
  "Return CODE-BLOCK of KEY in `tlon-init-code-overrides'.
When KEY is not present in `tlon-init-code-overrides', return the
default, non-overridden code. The variable
`tlon-init-code-overrides' is populated during the init process.

The syntax for the KEY parameter is `:ps/{package-name}' where
`{package-name}' is the name of the package. Examples:
`:ps/general', `:ps/embark', `:ps/hydra'.

Example usage:

\(tlon-init-override-code-if-available
 :ps/embark
 \='(
  (use-package embark
   ;; Default, non-overridden code goes here,
   ;; in this case, the full use-package call
   )))

If `:ps/embark' is found within `tlon-init-code-overrides' in this
example, the default will be overridden by that code."
  (with-temp-buffer
    (dolist (row (alist-get key tlon-init-code-overrides code-block))
      (insert (prin1-to-string row)))
    (eval-buffer)))

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
  "Return Alist of Chemacs profiles and associated init locations."
  ;; update `chemacs-profiles' in case a new profile was added
  ;; this is just the `defvar' of `chemacs-profiles' copied from chemacs.el
  (setq chemacs-profiles
	(with-temp-buffer
	  (insert-file-contents chemacs-profiles-path)
	  (goto-char (point-min))
	  (condition-case err
	      (read (current-buffer))
	    (error
	     (error "Failed to parse %s: %s" chemacs-profiles-path (error-message-string err))))))
  ;; now return an alist of profile names and their associated init file locations
  (let (target-directories)
    (dolist (chemacs-profile chemacs-profiles target-directories)
      (push
       (cons
	(car chemacs-profile)
	(cdadr chemacs-profile))
       target-directories))))

(defun user-pablo-p ()
  "Check if the current user is Pablo."
  (string= user-full-name "Pablo Stafforini"))

(defun tlon-init-set-tangle-flags (init-dir)
  "Set tangle flags for INIT-DIR."
  (let ((tangle-flags-filename (file-name-concat init-dir "tangle-flags.el")))
    (condition-case err
	(setq tlon-init-tangle-flags (tlon-init-read-file tangle-flags-filename))
      (error err
	     (setq tlon-init-tangle-flags nil)))
    (if tlon-init-tangle-flags
	(message (concat "Re-read init tangle flags from filename: " tangle-flags-filename))
      (unless (user-pablo-p)
	(user-error "`tangle-flags.el' not present present in init dir.")))))

(defun tlon-init-build (init-dir)
  "Build or rebuild a profile in INIT-DIR."
  (interactive
   (list
    (tlon-init-profile-dir
     (completing-read
      "Select Chemacs profile to build: "
      (tlon-init-available-init-dirs)))))
  (unless (string-equal major-mode "org-mode")
    (user-error "Error: cannot build init from a buffer that is not visiting an `org-mode' file"))
  ;; set paths for code blocks
  (setq tlon-init-user-init-path (file-name-concat init-dir "init-pablo.el")
	tlon-init-early-init-path (file-name-concat init-dir "early-init.el")
	tlon-init-variables-path (file-name-concat init-dir "variables.el")
	tlon-init-code-overrides-path (file-name-concat init-dir "code-overrides.el")
	tlon-init-post-init-path (file-name-concat init-dir "post-init.el")
	tlon-init-tangle-flags-path (file-name-concat init-dir "tangle-flags.el")
	tlon-init-variables-override-path (file-name-concat init-dir "variables-override.el"))
  ;; conditionally tangle extra config file
  (unless (user-pablo-p)
    (tlon-init-tangle-extra-config-file))
  (tlon-init-set-tangle-flags init-dir)
  ;; tangle `config.org'
  (tlon-init-tangle)
  ;; conditionally tangle extra config file again
  ;; TODO: implement more elegant solution; this is
  ;; a hack to deal with precedence problems
  (unless (user-pablo-p)
    (tlon-init-tangle-extra-config-file)))

(defun tlon-init-tangle ()
  "Tangle `config.org'."
  (widen)
  (save-buffer)
  ;; decrypt, then re-encrypt "variables" heading
  (org-decrypt-entries)
  (let ((org-babel-pre-tangle-hook (remove 'save-buffer org-babel-pre-tangle-hook)))
    (org-babel-tangle))
  (org-encrypt-entries)
  (save-buffer)
  (message "Tangled init files to Chemacs profile `%s'" tlon-init-user-init-path))

(defun tlon-init-tangle-extra-config-file ()
  "Tangle extra config file."
  (let* ((user-first-name (downcase (car (split-string user-full-name))))
	 (extra-config-file (file-name-concat default-directory
					      (concat "config-" user-first-name ".org"))))
    (if (file-exists-p extra-config-file)
	(with-current-buffer (or (find-file-noselect extra-config-file)
				 (find-buffer-visiting extra-config-file))
	  (tlon-init-tangle))
      (user-error "Extra config file for user %s not found" user-first-name))))

(defun tlon-init-eval-value-if-possible (value)
  "Evaluate variable VALUE if possible, else return unevaluated VALUE."
  (condition-case err
      (eval value)
    (error value)))

(defun tlon-init-load-variables ()
  "Load or re-load variables and from the currently booted init profile."
  (interactive)
  (let ((default-vars
	 (tlon-init-read-file
	  (eval (alist-get :variables-default tlon-init-filenames))))
	(override-vars
	 (tlon-init-read-file
	  (eval (alist-get :variables-override tlon-init-filenames)))))
    ;; set all variables in :variables-default, overriding with values from :variables-override when present
    (dolist (row default-vars)
      (set
       (car row)
       (tlon-init-eval-value-if-possible
	(alist-get (car row) override-vars (cdr row)))))
    ;; set variables from :variables-override that are not present in :variables-default
    (dolist (row override-vars)
      (unless (symbolp (car row))
	(set
	 (car row)
	 (tlon-init-eval-value-if-possible (cdr row)))))))

(defun tlon-init-load-code-overrides ()
  "Load or re-load code overrides and from the currently booted init profile."
  (setq tlon-init-code-overrides
	(tlon-init-read-file (eval (alist-get :code-overrides tlon-init-filenames)))))

(defun tlon-init-profile-dir (profile-name)
  "Return the directory of the Chemacs profile PROFILE-NAME."
  (alist-get profile-name (tlon-init-available-init-dirs) nil nil 'string=))

(defun tlon-init-act-on-chemacs-profiles (profile-name &optional profile-dir action)
  "Create, delete or set PROFILE-NAME as default in PROFILE-DIR.
When ACTION is `'set-default', set PROFILE-NAME as default.
When ACTION is `'create', create PROFILE-NAME.
Otherwise, delete PROFILE-NAME."
  (let* ((emacs-profiles (file-truename "~/.emacs-profiles.el"))
	 (regex-default (format "(\"default\" . ((user-emacs-directory . \"%s\")))" (tlon-init-profile-dir "default")))
	 (regex-search (if (member action '(create set-default))
			   "(\"default\" . ((user-emacs-directory . \".+?\")))"
			 (format "(\"%s\" . ((user-emacs-directory . \".+?\")))" profile-name)))
	 (regex-replace (pcase action
			  ('create
			   (concat regex-default "\n"
				   (format "(\"%s\" . ((user-emacs-directory . \"%s\")))" profile-name profile-dir)))
			  ('set-default
			   (format "(\"default\" . ((user-emacs-directory . \"%s\")))" profile-dir))
			  (_
			   ""))))
    (with-current-buffer (or (find-buffer-visiting emacs-profiles)
			     (find-file-noselect emacs-profiles))
      (goto-char (point-min))
      (re-search-forward regex-search nil t)
      (replace-match regex-replace)
      (delete-blank-lines)
      (save-buffer))))

(defun tlon-init-profile-exists-p (profile-name)
  "Return non-nil if Chemacs profile PROFILE-NAME exists."
  (when-let ((profile-dir (tlon-init-profile-dir profile-name)))
    (file-directory-p profile-dir)))

(defun tlon-init-create-profile (profile-name &optional overwrite)
  "Create a new Chemacs profile named PROFILE-NAME.
This adds a new profile to `~/.emacs-profiles.el' and creates a
 directory in the Chemacs profiles directory. The directory will
 have PROFILE-NAME as its name. If profile already exists, throw
 a user error message, unless OVERWRITE is non-nil."
  (let ((profile-dir (file-name-concat
		      (file-name-directory (directory-file-name user-emacs-directory))
		      profile-name)))
    (when (string-match file-name-invalid-regexp profile-name)
      (user-error "Invalid profile name"))
    (when (and (tlon-init-profile-exists-p profile-name)
	       (not overwrite))
      (user-error "Profile already exists"))
    (make-directory profile-dir t)
    (tlon-init-act-on-chemacs-profiles profile-name profile-dir 'create)
    (message "Created new Chemacs profile `%s'. Default profile is `%s'"
	     profile-name
	     (file-name-nondirectory (tlon-init-profile-dir "default")))
    profile-name))

(defun tlon-init-delete-profile (profile-name)
  "Delete a Chemacs profile with name PROFILE-NAME."
  (interactive
   (list (completing-read "Chemacs profile name to delete: "
			  (mapcar 'car (tlon-init-available-init-dirs)))))
  (when (string= profile-name chemacs-profile-name)
    (unless (y-or-n-p (format "You have chosen to delete profile %s, which is currently active. Proceed? "
			      profile-name))
      (user-error "Aborted")))
  (let ((profile-dir (tlon-init-profile-dir profile-name)))
    ;; first delete profile dir, if it exists
    (when (and (file-exists-p profile-dir)
	       (y-or-n-p (format "Are you sure you want to delete the directory '%s'? "
				 (tlon-init-profile-dir profile-name))))
      (delete-directory profile-dir t t))
    ;; then delete profile from ~/.emacs-profiles.el
    (tlon-init-act-on-chemacs-profiles profile-name profile-dir 'delete)
    (message "Deleted Chemacs profile '%s'" profile-name)
    (when (string= profile-dir (tlon-init-profile-dir "default"))
      (call-interactively 'tlon-init-set-default-profile))))

(defun tlon-init-set-default-profile (profile-name)
  "Set the default Chemacs profile to PROFILE-NAME."
  (interactive
   (list (completing-read "Chemacs profile name to set as new default: "
			  (mapcar 'car (tlon-init-available-init-dirs)))))
  (let ((profile-dir (tlon-init-profile-dir profile-name)))
    (when (not (file-exists-p profile-dir))
      (user-error "Profile does not exist"))
    (tlon-init-act-on-chemacs-profiles profile-name profile-dir 'set-default)
    (message "Set default Chemacs profile to '%s'" profile-name)))

(defun tlon-init-deploy-profile (profile-name)
  "Deploy PROFILE-NAME.
If you are deploying a new profile in a machine with `tlon-init'
managed by `elpaca', you only need to run this command.
Otherwise, you must first clone
https://github.com/tlon-team/tlon-init, open `tlon-init.el', and
`M-x eval-buffer'.

If there is already a `tlon-init' subdirectory in the `elpaca'
`repos' directory, you will be asked to confirm you want to
overwrite it. This will also overwrite the `init.el' file in the
profile directory, if such a file is found, but will leave the
rest of the profile intact. To delete the entire profile, use
`tlon-init-delete-profile'."
  (interactive "sProfile name: ")
  (if (not (tlon-init-profile-exists-p profile-name))
      (tlon-init-create-profile profile-name t)
    (if (y-or-n-p (format "Profile `%s' already exists. Redeploy? " profile-name))
	(setq overwrite t)
      (user-error "Deploy aborted")))
  (let* ((profile-dir (tlon-init-profile-dir profile-name))
	 (overwrite nil)
	 (package-dir (file-name-concat profile-dir "elpaca/repos/tlon-init/"))
	 (init-file-source (file-name-concat package-dir
					     (if (user-pablo-p)
						 "tlon-init-without-overrides.el"
					       "tlon-init-with-overrides.el")))
	 (init-file-target (file-name-concat profile-dir "init.el"))
	 (tlon-init-repo "https://github.com/tlon-team/tlon-init"))
    (when (file-exists-p package-dir)
      (if (or overwrite
	      (y-or-n-p (format "`%s' is not empty. Overwrite? " package-dir)))
	  (progn
	    (delete-directory package-dir t t)
	    (setq overwrite t))
	(user-error "Deploy aborted")))
    (shell-command (format "git clone %s %s" tlon-init-repo package-dir))
    (when (file-exists-p init-file-target)
      (if (or overwrite
	      (y-or-n-p (format "`%s' already exists. Overwrite? " init-file-target)))
	  (delete-file init-file-target)
	(user-error "Deploy aborted")))
    (copy-file init-file-source init-file-target t)
    (let ((message (format "Deployed profile '%s' to '%s'." profile-name profile-dir)))
      (if (and (boundp 'ps/file-config)
	       (y-or-n-p (concat message " Build init files?")))
	  (with-current-buffer (or (find-file-noselect ps/file-config)
				   (find-buffer-visiting ps/file-config))
	    (tlon-init-build profile-dir))
	(message message)))))

(provide 'tlon-init)

;;; tlon-init.el ends here
