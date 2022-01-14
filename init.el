;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2022-01-14 14:12:21 ndegruchy>

;; Initialize the package manager
(package-initialize)

;; Load the local lisp directory
(add-to-list 'load-path (concat user-emacs-directory "settings.d/"))

;; Load Private Variables
(load-file (concat user-emacs-directory "site-lisp.d/private.el"))

;; Different config parts
(load-library "general-settings")
(load-library "required-packages")
(load-library "included-packages")
(load-library "custom-functions")
(load-library "custom-keybindings")

;; Set custom file
;; Brilliant hack to effectively discard this file
;; Found on 2020-08-12
;; at: https://github.com/cmacrae/.emacs.d#discard-customizations
(setq custom-file (make-temp-file ""))
