;; included-packages.el
;; Configure packages distributed with Emacs

;; Ido
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-vertical-define-keys 'C-n-C-p-up-and-down)
(ido-mode 1)
(ido-vertical-mode 1)

(require 'midnight)
(midnight-delay-set 'midnight-delay 16200)

(use-package org
  :bind
  ("C-c R" . ndegruchy/org-replace-link-by-link-description))

;; Uniquify
(setq uniquify-buffer-name-style    'reverse
      uniquify-separator            "/"
      uniquify-after-kill-buffer-p  t
      uniquify-ignore-buffers-re    "^\\*")

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
