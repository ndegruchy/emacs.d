;; included-packages.el  -*- truncate-lines: t; -*-
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
							 (dired-hide-details-mode)))
(setq dired-listing-switches "--almost-all --ignore-backups --dired --human-readable -l --group-directories-first --sort=extension"
	  dired-dwim-target t)

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
(require 'ibuffer)
(require 'ibuf-ext) ;; for hiding buffers in the list
(setq-default ibuffer-saved-filter-groups
			  `(("nathan"
				 ("Version Control" (or (name . "^\*changes to .*")
										(name . "^\*vc\*")))
				 ("Dired" (mode . dired-mode))
				 ("Org" (mode . org-mode))
				 ("Emacs Config" (or (name . "\.el$")
									 (name . "\.el.gz$")))
				 ("Web" (or (mode . html-mode)
							(mode . css-mode)
							(mode . mhtml-mode)))
				 ("Calendar" (or (name . "^\\*Calendar\\*$")
								 (name . "^diary$")))
				 ("Help" (or (name . "\*Help\*")
							 (name . "\*Apropos\*")
							 (name . "\*Info\*")))
				 ("Temp" (name . "\*.*\*"))
				 ("Databases" (mode . rec-mode))
				 ("Modified" (predicate buffer-modified-p (current-buffer))))))

(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; I don't need to see these buffers, generally because they're not
;; useful to visit later
(setq ibuffer-never-show-predicates
	  (mapcar #'regexp-quote '("*Completions*"
							   "^*vc"
							   "*log-edit-files*")))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)
(setq ibuffer-use-header-line nil)
(setq ibuffer-display-summary nil)
(setq ibuffer-use-other-window t)
(setq ibuffer-movement-cycle t)
(setq ibuffer-formats
	  '((mark modified read-only locked " "
              (name 30 30 :left :elide)
              " "
              (size-h 9 12 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)))

(defun ndegruchy/my-ibuffer-hook ()
  "Custom hook for initializing ibuffer"
  (ibuffer-switch-to-saved-filter-groups "nathan")
  (hl-line-mode 1)
  (ibuffer-auto-mode 1)
  
  ;; I don't like it when it reuses the same buffer, though I need to
  ;; figure out how to make it also close the ibuffer... buffer when
  ;; selection occurs
  ;; TODO: Fix this
  (define-key ibuffer-mode-map (kbd "RET") 'ibuffer-visit-buffer-other-window))

(add-hook 'ibuffer-mode-hook 'ndegruchy/my-ibuffer-hook)

;; Org Mode
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook (lambda ()
						   (require 'org-tempo)))
(setq org-return-follows-link t)
(setq org-publish-project-alist
	  '(("notes"
		 :base-directory (concat (getenv "HOME") "/Documents/Notes")
		 :publishing-function org-html-publish-to-html
		 :publishing-directory (concat (getenv "HOME") "/Documents/Public/Notes")
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
