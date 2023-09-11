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

;;;; Requirements

;;;; Variables

;;;;; Package variables

(defvar tlon-init-early-init-path nil
  "Path to `early-init.el'.")

(defvar tlon-init-user-init-path nil
  "Path to user init file.")

(defvar tlon-init-variables-path nil
  "Path to `variables.el'.")

(defvar tlon-init-code-overrides-path nil
  "Path to `code-overrides.el'.")

(defvar tlon-init-post-init-path nil
  "Path to `post-init.el'.")

(defvar tlon-init-tangle-flags-path nil
  "Path to `tangle-flags.el'.")

(defvar tlon-init-variables-override-path nil
  "Path to `variables-override.el'.")

(defvar tlon-init-tangle-flags '()
  "Alist of tangle flags for each package.")

(defvar tlon-init-code-overrides '()
  "Alist of code overrides for each package.")

(defvar tlon-init-extra-config-tangle-pass nil
  "Pass number for extra config tangle.")

;;;;; User variables

;; We declare all the variables here and then Pablo sets their values
;; in his config file. Other users may in turn override these values.

(defvar ps/personal-name)
(defvar ps/personal-gmail)
(defvar ps/personal-email)
(defvar ps/tlon-email)
(defvar ps/dir-roo)
(defvar ps/dir-system-apps)
(defvar ps/dir-use)
(defvar ps/dir-dropbox)
(defvar ps/dir-downloads)
(defvar ps/dir-emacs)
(defvar ps/dir-google-drive)
(defvar ps/dir-music)
(defvar ps/dir-movies)
(defvar ps/dir-finance)
(defvar ps/dir-audiobooks)
(defvar ps/dir-music-tango)
(defvar ps/dir-music-popula)
(defvar ps/dir-music-classical)
(defvar ps/dir-music-to-so)
(defvar ps/dir-anki)
(defvar ps/dir-archive)
(defvar ps/dir-inactive)
(defvar ps/dir-personal-bibliography)
(defvar ps/dir-personal-csl-styles)
(defvar ps/dir-personal-csl-locales)
(defvar ps/dir-blog)
(defvar ps/dir-journal)
(defvar ps/dir-wiki)
(defvar ps/dir-wiki-entries)
(defvar ps/dir-wiki-references)
(defvar ps/dir-dotfiles)
(defvar ps/dir-dotemacs)
(defvar ps/dir-karabine)
(defvar ps/dir-bibliographic-notes)
(defvar ps/dir-yasnippets)
(defvar ps/dir-yasnippets-private)
(defvar ps/dir-abbrev)
(defvar ps/dir-private)
(defvar ps/dir-ledge)
(defvar ps/dir-notes)
(defvar ps/dir-people)
(defvar ps/dir-android)
(defvar ps/dir-bbdb)
(defvar ps/dir-ade)
(defvar ps/dir-library-pdf)
(defvar ps/dir-library-html)
(defvar ps/dir-library-media)
(defvar ps/dir-emacs-va)
(defvar ps/dir-source)
(defvar ps/dir-translation-serve)
(defvar ps/dir-PW)
(defvar ps/dir-org-clock-reports)
(defvar ps/dir-google-drive-tlo)
(defvar ps/dir-google-drive-tlon-leo)
(defvar ps/dir-google-drive-tlon-BAE)
(defvar ps/dir-google-drive-tlon-EA)
(defvar ps/dir-google-drive-tlon-FM)
(defvar ps/dir-google-drive-tlon-GPE)
(defvar ps/dir-google-drive-tlon-HEA)
(defvar ps/dir-google-drive-tlon-LBDLH)
(defvar ps/dir-google-drive-tlon-LP)
(defvar ps/dir-google-drive-tlon-RAE)
(defvar ps/dir-google-drive-tlon-RCG)
(defvar ps/dir-dropbox-tlo)
(defvar ps/dir-google-drive-tlon-core)
(defvar ps/dir-google-drive-tlon-fede)
(defvar ps/dir-dropbox-tlon-core)
(defvar ps/dir-dropbox-tlon-leo)
(defvar ps/dir-dropbox-tlon-fede)
(defvar ps/dir-dropbox-tlon-pablo)
(defvar ps/dir-dropbox-tlon-ledge)
(defvar ps/dir-dropbox-tlon-pass)
(defvar ps/dir-dropbox-tlon-BAE)
(defvar ps/dir-dropbox-tlon-EA)
(defvar ps/dir-dropbox-tlon-FM)
(defvar ps/dir-dropbox-tlon-GPE)
(defvar ps/dir-dropbox-tlon-HEA)
(defvar ps/dir-dropbox-tlon-LBDLH)
(defvar ps/dir-dropbox-tlon-LP)
(defvar ps/dir-dropbox-tlon-RAE)
(defvar ps/dir-dropbox-tlon-RCG)
(defvar ps/dir-repos)
(defvar ps/dir-tlon-babel)
(defvar ps/dir-tlon-docs)
(defvar ps/dir-emacs-local)
(defvar ps/dir-chemacs-profiles)
(defvar ps/dir-org)
(defvar ps/dir-org-roam)
(defvar ps/dir-org-images)
(defvar ps/dir-websites)
(defvar ps/dir-calibre)
(defvar ps/dir-all-repos '())
(defvar ps/file-notes)
(defvar ps/file-inbox-desktop)
(defvar ps/file-inbox-mobile)
(defvar ps/file-calenda)
(defvar ps/file-feeds-pablo)
(defvar ps/file-tlon-feeds)
(defvar ps/file-anki)
(defvar ps/file-ini)
(defvar ps/file-config)
(defvar ps/file-karabine)
(defvar ps/file-karabiner-ed)
(defvar ps/file-personal-bibliography-old)
(defvar ps/file-personal-bibliography-new)
(defvar ps/file-wiki-notes)
(defvar ps/file-wiki-published)
(defvar ps/file-wiki-help)
(defvar ps/file-library)
(defvar ps/file-quotes)
(defvar ps/file-films)
(defvar ps/file-tlon-tareas-leo)
(defvar ps/file-tlon-tareas-fede)
(defvar ps/file-org2blog)
(defvar ps/file-straight-profile)
(defvar ps/file-orb-noter-template)
(defvar ps/file-orb-capture-template)
(defvar ps/file-bookmarks)
(defvar ps/file-ledge)
(defvar ps/file-ledger-db)
(defvar ps/file-metaculus)
(defvar ps/file-gpe)
(defvar ps/file-fm)
(defvar ps/file-ffrp)
(defvar ps/file-rcg)
(defvar ps/file-ea)
(defvar ps/file-cookies)
(defvar ps/file-work)
(defvar ps/file-tlon-ledger-schedule-file)
(defvar ps/file-tlon-babel)
(defvar ps/file-tlon-ledge)
(defvar ps/file-urls-open-externally-defaul)
(defvar ps/file-urls-open-externally-firefox)
(defvar ps/personal-bibliography-files)
(defvar ps/tlon-bibliography-files)
(defvar ps/all-bibliography-files)
(defvar ps/face-fixed-pitch)
(defvar ps/face-fixed-pitch-size)
(defvar ps/face-variable-pitch)
(defvar ps/monitor-type)
(defvar ps/ledger-active-currencies)
(defvar ps/frame-width-threshold)
(defvar ps/new-empty-buffer-major-mode)
(defvar ps/forge-owned-accounts)
(defvar ps/personal-signature)
(defvar ps/location-name)
(defvar ps/display-wttr-locations)
(defvar ps/location-latitude)
(defvar ps/location-longitude)
(defvar ps/split-width-threshold)
(defvar ps/telega-server-libs-prefix)
(defvar ps/init-locatio)
(defvar ps/early-init-locatio)
(defvar ps/init-target-locations)

;;;; Functions

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

(defun tlon-init-user-pablo-p ()
  "Return t if Pablo is the current user, and nil otherwise."
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
      (unless (tlon-init-user-pablo-p)
	(user-error "`tangle-flags.el' not present present in init dir")))))

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
  ;; conditionally tangle extra config file, pass 1: get tangle flags only
  (setq tlon-init-extra-config-tangle-pass 1)
  (unless (tlon-init-user-pablo-p)
    (tlon-init-tangle-extra-config-file))
  (tlon-init-set-tangle-flags init-dir)
  ;; tangle `config.org'
  (tlon-init-tangle)
  ;; conditionally tangle extra config file, pass 2: get the rest of extra config
  (setq tlon-init-extra-config-tangle-pass 2)
  (unless (tlon-init-user-pablo-p)
    (tlon-init-tangle-extra-config-file)))

(defun tlon-init-tangle ()
  "Tangle `config.org'."
  (widen)
  (save-buffer)
  (let ((org-babel-pre-tangle-hook (remove 'save-buffer org-babel-pre-tangle-hook)))
    (org-babel-tangle))
  (save-buffer)
  (message "Tangled init files to Chemacs profile `%s'" tlon-init-user-init-path))

(defun tlon-init-get-variables (org-id)
  "Get contents of `shared variables' in heading with ORG-ID."
  (org-id-goto org-id)
  (save-restriction
    (org-narrow-to-subtree)
    (org-end-of-meta-data t)
    (buffer-substring-no-properties (point) (point-max))))

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
https://github.com/tlon-team/tlon-init, open `tlon-init.el' in
the cloned repo, and `M-x eval-buffer'.

If there is already a `tlon-init' subdirectory in the `elpaca'
`repos' directory, you will be asked to confirm you want to
overwrite it. This will also overwrite the `init.el' file in the
profile directory, if such a file is found, but will leave the
rest of the profile intact. To delete the entire profile, use
`tlon-init-delete-profile'."
  (interactive "sProfile name: ")
  (let ((overwrite nil))
    (if (not (tlon-init-profile-exists-p profile-name))
	(tlon-init-create-profile profile-name t)
      (if (y-or-n-p (format "Profile `%s' already exists. Redeploy? " profile-name))
	  (setq overwrite t)
	(user-error "Deploy aborted")))
    (let* ((profile-dir (tlon-init-profile-dir profile-name))
	   (package-dir (file-name-concat profile-dir "elpaca/repos/tlon-init/"))
	   (init-file-source (concat package-dir
				     (if (tlon-init-user-pablo-p)
					 "tlon-init-without-overrides.el"
				       "tlon-init-with-overrides.el")))
	   (init-file-target (file-name-concat profile-dir "init.el"))
	   (tlon-init-repo "git@github.com:tlon-team/tlon-init"))
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
	  (message message))))))

(provide 'tlon-init)

;;; tlon-init.el ends here
