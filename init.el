;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2018-07-09 19:13:08 ndegruchy>

;; Initialize the package manager
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell undo-tree auctex magit base16-theme avy use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
