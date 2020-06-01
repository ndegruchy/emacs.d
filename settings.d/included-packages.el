;; included-packages.el
;; Configure packages distributed with Emacs

(use-package abbrev
  :diminish abbrev-mode)

(use-package dired
  :bind (:map dired-mode-map
	      ;; Reuse the same dired window
	      ("RET" . dired-find-alternate-file)
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
  :config
  (setq dired-listing-switches "--group-directories-first -alh"
	dired-dwim-target      t)
  (setq-default dired-omit-files-p t)
  (require 'dired-x)
  (require 'dired+)
  (put 'dired-find-alternate-file 'disabled nil)
  :hook (dired-mode . dired-hide-details-mode))

(use-package ido
  :config
  (setq ido-enable-flex-matching t
	ido-everywhere t
	ido-vertical-define-keys 'C-n-C-p-up-and-down
	ido-auto-merge-work-directories-length -1)
  (ido-mode 1)
  (ido-vertical-mode 1))

(use-package ispell
  :config
  (setq ispell-program-name "aspell"
	ispell-extra-args '("--sug-mode=ultra")))

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "02:00am"))

(use-package org
  :defer t
  :config
  (setq org-directory          "~/Documents/Notes/"
	org-agenda-files       (list "index.org" "todo.org" "unsorted.org")
	org-archive-location   "archive.org"
	org-default-notes-file (concat org-directory "unsorted.org")
	org-capture-templates  (quote
				;; Create a new todo entry
				(("t" "todo" entry (file "todo.org")
				  "* TODO %?\n%U\n%a\n")
				;; Create a new generic note
				 ("n" "notes" entry (file "unsorted.org")
				  "* %? :NOTE:\n%U")
				;; Create a new read later entry
				 ("r" "read-later" entry (file "unsorted.org")
				  "** %? :READ:\n%U")))
	org-outline-path-complete-in-steps nil
	org-outline-use-outline-path 'file
	;; Figured out how to allow refile across files from this post:
	;; https://stackoverflow.com/questions/22200312/refile-from-one-file-to-other#22200624
	org-refile-targets      '((nil :maxlevel . 3)
				  (org-agenda-files :maxlevel . 3)))

  :bind (("C-c o" . org-capture)
	 ("C-c l" . org-store-link)))

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
