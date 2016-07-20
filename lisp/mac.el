;; Mac customizations

;; Bindings
(global-set-key (kbd "s-s") 'save-buffer)

;; Packages
(use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))
