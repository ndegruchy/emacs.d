;; included-packages.el
;; Configure packages distributed with Emacs

(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t
	ido-everywhere t
	ido-vertical-define-keys 'C-n-C-p-up-and-down)
  (ido-mode 1)
  (ido-vertical-mode 1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style    'reverse
        uniquify-separator            "/"
        uniquify-after-kill-buffer-p  t
        uniquify-ignore-buffers-re    "^\\*"))
