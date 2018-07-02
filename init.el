;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2018-07-02 12:25:09 ndegruchy>

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Load the local lisp directory
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/custom")

;; Different config parts
(load-library "settings")
(load-library "packages")
(load-library "functions")
(load-library "keybindings")
(load-library "hooks")
(load-library "hydras")

(when (file-exists-p custom-file)
  (load custom-file)) ;; For custom.el

;; Load platform-specific customizations

(when (eq system-type 'darwin)
  (load-library "mac"))

(when (eq system-type 'windows-nt)
  (load-library "windows"))

(when (eq system-type 'gnu/linux)
  (load-library "linux"))

(when (eq system-name "degruchy-crbk")
  (load-library "chromebook"))
