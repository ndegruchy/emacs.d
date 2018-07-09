;; included-packages.el
;; Configure packages distributed with Emacs

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style    'reverse
        uniquify-separator            "/"
        uniquify-after-kill-buffer-p  t
        uniquify-ignore-buffers-re    "^\\*"))
