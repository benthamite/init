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
When KEY is not present in `ps/inir-code-overrides', return the
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

(defun tlon-init-set-tangle-options (init-dir)
  "Set the tangle options for the files in INIT-DIR."
  ;; set target chemacs profile
  (setq tlon-init-user-init-path (file-name-concat init-dir "init-pablo.el"))
  (setq tlon-init-early-init-path (file-name-concat init-dir "early-init.el"))
  (setq tlon-init-variables-path (file-name-concat init-dir "variables.el"))
  (setq ps/code-overrides-path (file-name-concat init-dir "code-overrides.el"))
  (setq ps/post-init-path (file-name-concat init-dir "post-init.el"))
  (setq ps/tangle-flags-path (file-name-concat init-dir "tangle-flags.el"))
  (setq ps/variables-override-path (file-name-concat init-dir "variables-override.el"))
  (message "Set init tangle targets to: %s and %s" tlon-init-user-init-path tlon-init-early-init-path)
  ;; re-read tangle flags for that process
  (let ((tangle-flags-filename (file-name-concat init-dir "tangle-flags.el")))
    (condition-case err
	(setq tlon-init-tangle-flags (tlon-init-read-file tangle-flags-filename))
      (error err
	     (setq tlon-init-tangle-flags nil)))
    (if tlon-init-tangle-flags
	(message (concat "Re-read init tangle flags from filename: " tangle-flags-filename))
      (message "tangle-flags.el not present present in init dir. This is not necessarily a problem."))))

(defun tlon-init-build (init-dir)
  "Prompt user for a Chemacs profile, configure it and build it.
The selected Chemacs profile sets the INIT-DIR.

This command performs 4 successive actions:

1) Deploy or re-deploy all files related to init to the selected
Chemacs profile, overwriting older kept-new versions.

2) Set the selected Chemacs profile as the target for the tangle
output.

3) Read or re-read the tangle flags configuration for that
profile, so that code excluded via a tangle flag is not tangled.

4) Tangle `config.org' to the selected Chemacs profile.

If invoked with a prefix argument, copy `straight-profile.el'
from the dotemacs repo to the selected Chemacs profile directory."
  (interactive
   (list
    (tlon-init-profile-dir
     (completing-read
      "Select Chemacs profile to build: "
      (tlon-init-available-init-dirs)))))
  (if (not (string-equal major-mode "org-mode"))
      (message "Error: cannot build init from a buffer that is not visiting an `org-mode' file")
    ;; re-deploy files
    (message "Re-deploying init files to %s" init-dir)
    (tlon-init-deploy-profile (file-name-nondirectory init-dir))
    ;; NOTE: this is commented out until `elpaca' implements lockfile support
    ;; copy lockfile if missing or if user requested it
    ;; (let ((lockfile "straight-profile.el"))
    ;; (unless (and (file-exists-p (file-name-concat init-dir lockfile))
    ;; (equal current-prefix-arg nil))
    ;; (copy-file (file-name-concat ps/dir-dotemacs lockfile)
    ;; (file-name-concat init-dir lockfile)
    ;; t)))
    ;; set tangle options
    (tlon-init-set-tangle-options init-dir)
    ;; go ahead with the tangle
    (tlon-init-tangle init-dir)
    (unless (string= user-full-name "Pablo Stafforini")
      (tlon-init-tangle-extra-config-file init-dir))))

(defun tlon-init-tangle (init-dir)
  "Tangle `config.org' to INIT-DIR."
  (widen)
  (save-buffer)
  ;; decrypt, then re-encrypt "variables" heading
  (org-decrypt-entries)
  (let ((org-babel-pre-tangle-hook (remove 'save-buffer org-babel-pre-tangle-hook)))
    (org-babel-tangle))
  (org-encrypt-entries)
  (save-buffer)
  (message "Re-deployed and tangled init files to chemacs profile %s" init-dir))

(defun tlon-init-tangle-extra-config-file (init-dir)
  "Tangle extra config file for user to INIT-DIR."
  (let* ((user-first-name (downcase (car (split-string user-full-name))))
	 (extra-config-file (file-name-concat default-directory
					      (concat "config-" user-first-name ".org"))))
    (if (find-buffer-visiting extra-config-file)
	(with-current-buffer (or (find-file-noselect extra-config-file)
				 (find-buffer-visiting extra-config-file))
	  (tlon-init-tangle init-dir))
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

(defun tlon-init-replace-chemacs-profiles (profile-name &optional profile-dir action)
  "Create, delete or set PROFILE-NAME as default.
When ACTION is nil, delete PROFILE-NAME.
When ACTION is 'create, create PROFILE-NAME.
When ACTION is 'set-default, set PROFILE-NAME as default."
  (let* ((emacs-profiles (file-truename "~/.emacs-profiles.el"))
	 (regex-default (format "(\"default\" . ((user-emacs-directory . \"%s\")))" (tlon-init-profile-dir "default")))
	 (regex-search (if action
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

(defun tlon-init-create-profile (profile-name)
  "Create a new Chemacs profile with name PROFILE-NAME.
This adds a new profile to `~/.emacs-profiles.el' and creates a
 directory in the Chemacs profiles directory. The directory will
 have PROFILE-NAME as its name."
  (interactive "sNew Chemacs profile name: ")
  (let ((profile-dir (file-name-concat
		      (file-name-directory (directory-file-name user-emacs-directory))
		      profile-name)))
    (when (string-match file-name-invalid-regexp profile-name)
      (user-error "Invalid profile name"))
    (when (file-exists-p profile-dir)
      (user-error "Profile already exists"))
    (make-directory profile-dir t)
    (tlon-init-replace-chemacs-profiles profile-name profile-dir 'create)
    (message "Created new Chemacs profile `%s'. Default profile is `%s'"
	     profile-name
	     (file-name-nondirectory (tlon-init-profile-dir "default")))))

(defun tlon-init-delete-profile (profile-name)
  "Delete a Chemacs profile with name PROFILE-NAME."
  (interactive
   (list (completing-read "Chemacs profile name to delete: "
			  (mapcar 'car (tlon-init-available-init-dirs)))))
  (let ((profile-dir (tlon-init-profile-dir profile-name)))
    (when (not (file-exists-p profile-dir))
      (user-error "Profile does not exist"))
    (when (y-or-n-p (format "Are you sure you want to delete the profile '%s'? " profile-name))
      (delete-directory profile-dir t t)
      (tlon-init-replace-chemacs-profiles profile-name)
      (message "Deleted Chemacs profile '%s'" profile-name)
      (when (string= profile-dir (tlon-init-profile-dir "default"))
	(call-interactively 'tlon-init-set-default-profile)))))

(defun tlon-init-set-default-profile (profile-name)
  "Set the default Chemacs profile to PROFILE-NAME."
  (interactive
   (list (completing-read "Chemacs profile name to set as new default: "
			  (mapcar 'car (tlon-init-available-init-dirs)))))
  (let ((profile-dir (tlon-init-profile-dir profile-name)))
    (when (not (file-exists-p profile-dir))
      (user-error "Profile does not exist"))
    (tlon-init-replace-chemacs-profiles profile-name profile-dir 'set-default)
    (message "Set default Chemacs profile to '%s'" profile-name)))

(defun tlon-init-deploy-profile (profile-name &optional init-dir)
  "Deploy PROFILE-NAME in INIT-DIR."
  (interactive
   (list (completing-read "Chemacs profile to deploy: "
			  (mapcar 'car (tlon-init-available-init-dirs)))))
  (let* ((profile-dir (tlon-init-profile-dir profile-name))
	 (init-dir (or init-dir
		       (file-name-concat elpaca-repos-directory "new-init")))
	 (init-file (file-name-concat init-dir
				      (if (string= user-full-name "Pablo Stafforini")
					  "tlon-init-without-overrides.el"
					"tlon-init-with-overrides.el"))))
    (copy-file init-file (file-name-concat profile-dir "init.el") t)
    (let ((init-package-file "tlon-init.el"))
      (copy-file (file-name-concat init-dir init-package-file)
		 (file-name-concat profile-dir init-package-file)
		 t))))

(provide 'tlon-init)

;;; tlon-init.el ends here
