;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2020-06-07 00:00:49 nathan>

;; Initialize the package manager
(package-initialize)

;; Load the local lisp directory
(add-to-list 'load-path "~/.emacs.d/settings.d/")

;; Different config parts
(load-library "general-settings")
(load-library "required-packages")
(load-library "included-packages")
(load-library "custom-functions")
(load-library "custom-keybindings")
(load-library "custom-hooks")
(load-library "custom-aliases")
(load-library "custom-skeletons")

;; Set custom file
(setq custom-file "~/.emacs.d/custom-lisp.d/emacs-customize.el")
