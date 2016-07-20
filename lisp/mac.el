;; Mac customizations

;; Unbind
(global-unset-key (kbd "s-x"))

;; Binding
(global-set-key   (kbd "s-s") 'save-buffer)
(global-set-key   (kbd "s-x") 'smex)

;; Packages
(use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))
