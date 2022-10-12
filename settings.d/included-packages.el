;; included-packages.el
;; Configure packages distributed with Emacs

(use-package autoinsert
  :init
  (auto-insert-mode t)
  :config
  (define-auto-insert '(org-mode . "Basic Org Mode file") 'ndegruchy/skeleton-org-new-file)
  (define-auto-insert '(rec-mode . "Basic RecUtils database info") 'ndegruchy/skeleton-rec-new-file)
  (define-auto-insert '(sgml-mode . "Basic HTML template") 'ndegruchy/skeleton-web-new-file)
  (define-auto-insert '(html-mode . "Basic HTML template") 'ndegruchy/skeleton-web-new-file))

(use-package dired
  :bind (:map dired-mode-map
			  ("RET" . dired-find-alternate-file)
			  ("^" . (lambda () (interactive) (find-alternate-file ".."))))
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  ;; mouse-1 is aliased to mouse-2 in dired-mode 0_o
  (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)

  (setq dired-listing-switches "--almost-all --ignore-backups --dired --human-readable -l --group-directories-first --sort=extension")
  (if (version< emacs-version "28.1")
	  (progn
		;; for dired-jump and dired-omit
		(require 'dired-x))))

(use-package eshell
  :init
  (setenv "TERM" "xterm-256color")
  :config
  (setq eshell-ls-initial-args '("--almost-all"
								 "--ignore-backups"
								 "--human-readable"
								 "-l" ;; -- "long listing"
								 "--group-directories-first"
								 "--color=always"
								 "--sort=extension")))

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

(use-package org
  :init
  (require 'org-tempo)
  :config
  (setq org-return-follows-link t)
  (setq org-publish-project-alist
		'(("notes"
		   :base-directory "/home/nathan/Documents/Notes"
		   :publishing-function org-html-publish-to-html
		   :publishing-directory "/home/nathan/Documents/Public/Notes"
		   :section-numbers nil
		   :with-toc nil))))

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
  (sgml-electric-tag-pair-mode t)
  ;; Discovered it here https://stackoverflow.com/questions/1666513/how-to-indent-4-spaces-under-sgml-mode
  (setq sgml-basic-offset 4))
