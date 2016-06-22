;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2016-06-21 21:50:46 ndegruchy>

;; Load the local lisp directory
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Different config parts
(load-library "settings")
(load-library "packages")
(load-library "functions")
(load-library "keybindings")
(load-library "hooks")

(when (eq system-type 'darwin) ;; mac specific settings
  (load-library "mac"))

(when (file-exists-p custom-file)
  (load custom-file)) ;; For custom.el
