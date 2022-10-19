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
(setq dired-listing-switches "--almost-all --ignore-backups --dired --human-readable -l --group-directories-first --sort=extension"
	  dired-dwim-target t)
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

;; Ibuffer
(setq-default ibuffer-saved-filter-groups
			  `(("nathan"
				 ("Modified" (predicate buffer-modified-p (current-buffer)))
				 ("Dired" (mode . dired-mode))
				 ("Org" (mode . org-mode))
				 ("Web" (or (mode . html-mode)
							(mode . css-mode)
							(mode . mhtml-mode)))
				 ("Help" (or (name . "\*Help\*")
							 (name . "\*Apropos\*")
							 (name . "\*Info\*")))
				 ("Temp" (name . "\*.*\*")))))

(setq ibuffer-show-empty-filter-groups nil
	  ibuffer-expert t)

(defun ndegruchy/my-ibuffer-hook ()
  "Custom hook for initializing ibuffer"
  (ibuffer-switch-to-saved-filter-groups "nathan")
  (ibuffer-auto-mode 1))

(add-hook 'ibuffer-mode-hook 'ndegruchy/my-ibuffer-hook)

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


;; Package.el
(setq package-name-column-width 40
	  package-version-column-width 14
	  package-status-column-width 12
	  package-archive-column-width 8)
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; sgml mode
(add-hook 'sgml-mode-hook 'sgml-electric-tag-pair-mode)
;; Discovered it here https://stackoverflow.com/questions/1666513/how-to-indent-4-spaces-under-sgml-mode
(setq sgml-basic-offset 4)
