;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2022-05-04 13:52:55 nathan>

;; Initialize the package manager
(package-initialize)

;; Load the local lisp directory
(add-to-list 'load-path (concat user-emacs-directory "settings.d/"))

;; Load Private Variables
(if
	;; I hate when I start a new emacs dir, there is always this missing file
	(not (file-exists-p (concat user-emacs-directory "site-lisp.d/private.el")))
	(load-file (concat user-emacs-directory "site-lisp.d/private.el"))
  (with-temp-buffer (write-file (concat user-emacs-directory "site-lisp.d/private.el"))))

;; Different config parts
(load-library "general-settings")
(load-library "required-packages")
(load-library "included-packages")
(load-library "custom-functions")
(load-library "custom-keybindings")
(load-library "custom-abbrevs.el")

;; Load Windows-specific stuff
(if
 (string-equal system-type "windows-nt")
	(load-library "win32-settings"))


;; Set custom file
;; Brilliant hack to effectively discard this file
;; Found on 2020-08-12
;; at: https://github.com/cmacrae/.emacs.d#discard-customizations
(setq custom-file (make-temp-file ""))
