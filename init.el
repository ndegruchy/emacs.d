;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2018-07-09 15:24:32 ndegruchy>

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Load the local lisp directory
(add-to-list 'load-path "~/.emacs.d/settings.d/")
(add-to-list 'load-path "~/.emacs.d/custom-lisp.d/")

;; Different config parts
(load-library "general-settings")
(load-library "required-packages")
(load-library "custom-functions")
(load-library "custom-keybindings")
(load-library "custom-hooks")

;; Load platform-specific customizations

(when (eq system-type 'darwin)
  (load-library "macos-platform-settings"))

(when (eq system-type 'windows-nt)
  (load-library "windows-platform-settings"))

(when (eq system-type 'gnu/linux)
  (load-library "linux-platform-settings"))

(when (string-equal system-name "degruchy-chrbk")
  (load-library "chromebook-settings"))
