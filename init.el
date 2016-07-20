;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2016-07-20 14:05:10 ndegruchy>

;; Load the local lisp directory
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Different config parts
(load-library "settings")
(load-library "packages")
(load-library "functions")
(load-library "keybindings")

(when (file-exists-p custom-file)
  (load custom-file)) ;; For custom.el

;; Load platform-specific customizations

(when (eq system-type 'darwin)
  (load-library "mac"))

(when (eq system-type 'windows-nt)
  (load-library "windows"))

(when (eq system-type 'gnu/linux)
  (load-library "linux"))
