;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2021-02-16 09:49:07 natha>

;; Initialize the package manager
(package-initialize)

;; Load the local lisp directory
(add-to-list 'load-path "~/.emacs.d/settings.d/")

;; Load Private Variables
(load-file "~/.emacs.d/site-lisp.d/private.el")

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
