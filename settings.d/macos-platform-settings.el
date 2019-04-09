;; macos-platform-settings.el
;; Set mac plaform specific settings here

;; Unbind
(global-unset-key (kbd "s-x"))

;; Binding
(global-set-key   (kbd "s-s") 'save-buffer)

;; Packages
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))
