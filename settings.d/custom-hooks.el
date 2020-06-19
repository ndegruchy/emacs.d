;; custom-hooks.el
;; Set custom mode or setting hooks here

;; Timestamps
(add-hook 'before-save-hook 'time-stamp)

;; Protect buffers
(add-hook 'after-init-hook #'protect-buffers)

;; Emmet
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

;; Markdown mode
(add-hook 'markdown-mode 'flyspell-mode)
(add-hook 'markdown-mode 'auto-fill-mode)

;; Org mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'flyspell-mode)
