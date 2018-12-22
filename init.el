;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2018-12-20 20:38:43 ndegruchy>

;; Initialize the package manager
(package-initialize)

;; Load the local lisp directory
(add-to-list 'load-path "~/.emacs.d/settings.d/")
(add-to-list 'load-path "~/.emacs.d/custom-lisp.d/")

;; Different config parts
(load-library "general-settings")
(load-library "required-packages")
(load-library "included-packages")
(load-library "custom-functions")
(load-library "custom-keybindings")
(load-library "custom-hooks")

;; Set custom file
(setq custom-file "~/.emacs.d/custom-lisp.d/emacs-customize.el")

;; Load platform-specific customizations

(when (eq system-type 'darwin)
  (load-library "macos-platform-settings"))

(when (eq system-type 'windows-nt)
  (load-library "windows-platform-settings"))

(when (eq system-type 'gnu/linux)
  (load-library "linux-platform-settings"))
