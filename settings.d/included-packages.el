;; included-packages.el
;; Configure packages distributed with Emacs

(diminish 'auto-revert-mode)

(use-package abbrev
  :diminish abbrev-mode)

(use-package appt
  :config
  (setq appt-audible t
		appt-display-mode-line t
		appt-display-format 'window)
  (appt-activate 1))

(use-package auth-source-pass
  :config
  (require 'auth-source-pass)
  (auth-source-pass-enable))

(use-package diary
  :hook (diary-list-entries-hook . (diary-sort-entries t)))

(use-package dired
  :bind (:map dired-mode-map
			  ;; Reuse the same dired window
			  ;; ("RET" . dired-find-alternate-file)
			  ("^"   . (lambda()
						 (interactive)
						 (find-alternate-file "..")))
			  ;; Use 'open' to open the file with the user's choice
			  ("E"   . ndegruchy/open-in-external-app)
			  ;; Get the file size(s)
			  ("; d" . dired-get-size)
			  ;; Toggle omit
			  ("; o" . dired-omit-mode)
			  ;; Close the frame, useful when using dired by itself
			  ("; q" . delete-frame))
  :hook (dired-mode . dired-hide-details-mode)
  :init
  (load-file "~/.emacs.d/site-lisp.d/dired+.el")
  (diredp-toggle-find-file-reuse-dir t)
  :config
  (setq-default dired-omit-files-p t)
  (setq dired-listing-switches "--group-directories-first -alh"
		dired-dwim-target t
		dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (put 'dired-find-alternate-file 'disabled nil))

(use-package eshell
  :config
  (setq eshell-visual-commands '("less" "tmux" "htop" "top" "bash" "fish" "zsh")
		eshell-visual-subcommands '(("git" "log" "l" "diff" "show"))
		eshell-ls-initial-args "-alh"))

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "02:00am"))

(use-package org
  :config
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (setq org-publish-project-alist
		'(("n.d.o"
		   :base-directory "~/Documents/Notes"
		   :recursive t
		   :publishing-directory "~/Documents/Notes/.www/"
		   :publishing-function org-html-publish-to-html))
		org-publish-use-timestamps-flag nil
		org-html-validation-link nil))

(use-package savehist
  :config
  (savehist-mode 1)
  (setq history-delete-duplicates t
		history-length 20))

(use-package sgml-mode
  :config
  ;; Discovered it here https://stackoverflow.com/questions/1666513/how-to-indent-4-spaces-under-sgml-mode
  (setq sgml-basic-offset 4))

;; Uniquify
(setq uniquify-buffer-name-style    'reverse
      uniquify-separator            "/"
      uniquify-after-kill-buffer-p  t
      uniquify-ignore-buffers-re    "^\\*")

;; Window moving keybindings
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
