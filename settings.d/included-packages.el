;;; included-packages.el --- My configs for the included emacs packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: local, data, processes, convenience

;; Autoinsert
(auto-insert-mode t)
(define-auto-insert '(org-mode . "Basic Org Mode file") 'ndegruchy/skeleton-org-new-file)
(define-auto-insert '(sgml-mode . "Basic HTML template") 'ndegruchy/skeleton-web-new-file)
(define-auto-insert '(html-mode . "Basic HTML template") 'ndegruchy/skeleton-web-new-file)

;; Dired
(add-hook 'dired-mode-hook 'ndegruchy/dired-mode-hook)
(setq dired-listing-switches "--almost-all --ignore-backups --dired --human-readable -l --group-directories-first --sort=extension"
	  dired-dwim-target t)
(setq dired-guess-shell-alist-user '((".*" "~/.local/bin/handlr open")))

;; Special keys
(global-set-key (kbd "C-M-s-f") 'dired)	; Use that otherwise useless 'office' key

(defun ndegruchy/dired-mode-hook ()
  "A hook for dired to diable the mouse, and to quickly jump to
wdired mode and some other stuff I like to have"
  (define-key dired-mode-map [mouse-2] nil)
  (define-key dired-mode-map (kbd "<f9>") 'wdired-change-to-wdired-mode)
  (dired-hide-details-mode)
  (hl-line-mode 1))

;; EShell
(setq eshell-ls-initial-args '("--almost-all"
							   "--ignore-backups"
							   "--human-readable"
							   "-l" ;; long listing
							   "--group-directories-first"
							   "--color=never"
							   "--sort=extension"))

;; Flyspell/ISpell
(when (executable-find "hunspell")
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-personal-dictionary (concat (getenv "XDG_DATA_HOME") "/share/hunspell/personal_en_US")))

;; Ibuffer
(require 'ibuffer)
(require 'ibuf-ext) ;; for hiding buffers in the list
(setq-default ibuffer-saved-filter-groups
			  `(("nathan"
				 ("Org" (mode . org-mode))
				 ("Emacs Config" (or (name . "\.el$")
									 (name . "\.el.gz$")
									 (mode . elisp-mode)))
				 ("Web" (or (mode . html-mode)
							(mode . css-mode)
							(mode . mhtml-mode)))
				 ("IRC" (or (mode . rcirc-mode)))
				 ("Dired" (mode . dired-mode))
				 ("Music" (or (mode . emms-browser-mode)
							  (mode . emms-playlist-mode)
							  (name . "^\*EMMS")))
				 ("Shell" (or (mode . eshell-mode)
							  (mode . shell-mode)))
				 ("Packages" (mode . package-menu-mode))
				 ("Calendar" (or (name . "^\\*Calendar\\*$")
								 (name . "^diary$")))
				 ("Help" (or (name . "\*Help\*")
							 (name . "\*Apropos\*")
							 (name . "\*Info\*")
							 (name . "^\*Man")
							 (mode . man-mode)))
				 ("Databases" (or (mode . rec-mode)
								  (mode . rec-summary-mode)))
				 ("Images" (or (mode . image-mode)
							   (mode . image-minor-mode)
							   (mode . auto-image-file-mode)))
				 ("Version Control" (or (name . "^\*changes to .*")
										(name . "^\*vc\*$")
										(mode . compilation-mode)
										(mode . change-log-mode)))
				 ("Temp" (name . "\*.*\*")))))

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

(defun ndegruchy/ibuffer-open-and-close ()
  "Switch to the selected buffer and close ibuffer"
  (interactive)
  (ibuffer-visit-buffer-other-window :noselect)
  (delete-window))

(defun ndegruchy/my-ibuffer-hook ()
  "Custom hook for initializing ibuffer"
  (ibuffer-switch-to-saved-filter-groups "nathan")
  (hl-line-mode 1)
  (ibuffer-auto-mode 1)
  ;; Make ibuffer close itself when selecting a buffer
  (define-key ibuffer-mode-map (kbd "RET") 'ndegruchy/ibuffer-open-and-close)
  (define-key ibuffer-mode-map (kbd "q") 'delete-window))

(add-hook 'ibuffer-mode-hook 'ndegruchy/my-ibuffer-hook)

;; Not really *ibuffer*, per-se, but close enough
(global-set-key (kbd "s-<left>")	'previous-buffer)
(global-set-key (kbd "s-<right>")	'next-buffer)

;; Isearch
(defun ndegruchy/isearch-clear-search ()
  "Deletes everything in the current isearch area"
  (interactive)
  (isearch-del-char most-positive-fixnum))
(define-key isearch-mode-map (kbd "C-S-<backspace>") #'ndegruchy/isearch-clear-search)

;; Occur Mode
;; Turn off line wrapping in occur-mode buffer(s)
(add-hook 'occur-mode-hook #'toggle-truncate-lines)
(add-hook 'occur-edit-mode-hook #'toggle-truncate-lines)

;; Org Mode
(defun ndegruchy/org-mode-hook ()
  "A hook for Org Mode for different parts that I'd like to enable"
  (require 'org-tempo))

(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'ndegruchy/org-mode-hook)

(setq org-return-follows-link t)
(setq org-publish-project-alist
	  '(("notes"
		 :base-directory (concat (getenv "HOME") "/Documents/Notes")
		 :publishing-function org-html-publish-to-html
		 :publishing-directory (concat (getenv "HOME") "/Documents/Public/Notes")
		 :section-numbers nil
		 :with-toc nil)))

;; Package.el
(setq package-name-column-width 30
	  package-version-column-width 14
	  package-status-column-width 12
	  package-archive-column-width 8)
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; RecentF
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 10)
(defun ndegruchy/recentf-mode-hook ()
  (hl-line-mode 1))
(add-hook 'recentf-dialog-mode-hook #'ndegruchy/recentf-mode-hook)

;; Remember
(global-set-key (kbd "<f7>") 'remember-notes)
(global-set-key (kbd "<f8>") 'remember)

;; Repeat mode
(repeat-mode 1)

;; SGML mode
(add-hook 'sgml-mode-hook 'sgml-electric-tag-pair-mode)
;; Discovered it here https://stackoverflow.com/questions/1666513/how-to-indent-4-spaces-under-sgml-mode
(setq sgml-basic-offset 4)

;; Local Variables:
;; truncate-lines: t
;; End:
