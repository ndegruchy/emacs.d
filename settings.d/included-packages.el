;; included-packages.el
;; Configure packages distributed with Emacs

(diminish 'auto-revert-mode)

(use-package dired
  :init
  (load-library (concat user-emacs-directory "site-lisp.d/dired+.el"))
  (diredp-toggle-find-file-reuse-dir t)
  :config
  (setq dired-listing-switches "--almost-all --ignore-backups --dired --human-readable -l --group-directories-first --sort=extension"))

(use-package eshell
  :config
  (setq eshell-visual-commands '("less" "tmux" "htop" "top" "bash" "fish" "zsh")
		eshell-visual-subcommands '(("git" "log" "l" "diff" "show"))
		eshell-ls-initial-args "-alh"))

(use-package flyspell
  :config
  (when (executable-find "hunspell")
	(setq ispell-program-name (executable-find "hunspell")
		  ispell-extra-args (list
							 "-d en_US"
							 (concat "-p " "~/.local/share/hunspell/personal-dict")))))

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "02:00am"))

(use-package proced
  :config
  (setq-default proced-auto-update-flag t
				proced-auto-update-interval 5))

(use-package savehist
  :config
  (savehist-mode 1)
  (setq history-delete-duplicates t
		history-length 20
		savehist-additional-variables '(helm-M-x-input-history
										extended-command-history)))

(use-package sgml-mode
  :config
  ;; Discovered it here https://stackoverflow.com/questions/1666513/how-to-indent-4-spaces-under-sgml-mode
  (setq sgml-basic-offset 4))
