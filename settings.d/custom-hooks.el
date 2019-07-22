;; custom-hooks.el
;; Set custom mode or setting hooks here

;; Timestamps
(add-hook 'before-save-hook 'time-stamp)

;; Protect buffers
(add-hook 'after-init-hook #'protect-buffers)
