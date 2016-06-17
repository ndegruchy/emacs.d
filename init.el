;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2016-06-17 16:39:51 ndegruchy>

;; Load the local lisp directory
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Different config parts
(load-library "settings")
(load-library "packages")
(load-library "functions")
(load-library "keybindings")
(load-library "hooks")
(load-library "custom") ;; For custom.el

;; Misc

;; Enable automatically pairing of parens, brackets, etc
(electric-pair-mode 1)

;; I sometimes am on a laptop, show the battery meter
(display-battery-mode +1)

;; Have IDO/Smex show recent files
(recentf-mode 1)
