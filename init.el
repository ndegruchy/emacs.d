;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2022-10-17 15:42:01 nathan>

;; Initialize the package manager
(package-initialize)

;; Load the local lisp directory
(add-to-list 'load-path (concat user-emacs-directory "settings.d/"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp.d/"))
(add-to-list 'load-path (concat user-emacs-directory "skeletons.d/"))

;; Load Private Variables
(if
	;; I hate when I start a new emacs dir, there is always this missing file
	(not (file-exists-p (concat user-emacs-directory "site-lisp.d/private.el")))
	(with-temp-buffer (write-file (concat user-emacs-directory "site-lisp.d/private.el")))
  (load-file (concat user-emacs-directory "site-lisp.d/private.el")))


;; Different config parts
(load-library "general-settings")
(load-library "required-packages")
(load-library "included-packages")
(load-library "custom-functions")
(load-library "custom-keybindings")
;; Skeletons
(load-library "web")
(load-library "org-mode")
(load-library "latex")
(load-library "rec")

;; UI Specific Stuff
(when (window-system)
  (load-library "gui-settings"))

;; Load Windows-specific stuff
(if
	(string-equal system-type "windows-nt")
	(load-library "win32-settings"))


;; Set custom file
;; Brilliant hack to effectively discard this file
;; Found on 2020-08-12
;; at: https://github.com/cmacrae/.emacs.d#discard-customizations
(setq custom-file (make-temp-file ""))
