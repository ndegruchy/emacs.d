;; included-packages.el
;; Configure packages distributed with Emacs

;; Autoinsert
(auto-insert-mode t)
(define-auto-insert '(org-mode . "Basic Org Mode file") 'ndegruchy/skeleton-org-new-file)
(define-auto-insert '(rec-mode . "Basic RecUtils database info") 'ndegruchy/skeleton-rec-new-file)
(define-auto-insert '(sgml-mode . "Basic HTML template") 'ndegruchy/skeleton-web-new-file)
(define-auto-insert '(html-mode . "Basic HTML template") 'ndegruchy/skeleton-web-new-file)

;; Dired
(add-hook 'dired-mode-hook (lambda ()
							 (define-key dired-mode-map [mouse-2] nil)
							 (dired-omit-mode)
							 (dired-hide-details-mode)))
(setq dired-listing-switches "--almost-all --ignore-backups --dired --human-readable -l --group-directories-first --sort=extension")
(if (version< emacs-version "28.1")
	(progn
	  ;; for dired-jump and dired-omit
	  (require 'dired-x)))

;; EShell
(setq eshell-ls-initial-args '("--almost-all"
							   "--ignore-backups"
							   "--human-readable"
							   "-l" ;; long listing
							   "--group-directories-first"
							   "--color=always"
							   "--sort=extension"))

;; Flyspell
(when (executable-find "hunspell")
  (setq ispell-program-name (executable-find "hunspell")
		ispell-extra-args (list
						   "-d en_US"
						   (concat "-p " (getenv "XDG_DATA_HOME") "/hunspell/personal-dict"))))

;; Org Mode
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook (lambda ()
						   (require 'org-tempo)))
(setq org-return-follows-link t)
(setq org-publish-project-alist
	  '(("notes"
		 :base-directory "/home/nathan/Documents/Notes"
		 :publishing-function org-html-publish-to-html
		 :publishing-directory "/home/nathan/Documents/Public/Notes"
		 :section-numbers nil
		 :with-toc nil)))

;; SGML mode
(add-hook 'sgml-mode-hook 'sgml-electric-tag-pair-mode)
;; Discovered it here https://stackoverflow.com/questions/1666513/how-to-indent-4-spaces-under-sgml-mode
(setq sgml-basic-offset 4)
