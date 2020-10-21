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
		eshell-visual-subcommands '(("git" "log" "l" "diff" "show"))))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)
		 ("C-x b"   . ibuffer))
  :init
  (add-hook 'ibuffer-hook #'ib-switch-to-saved-hook)
  (defun ib-switch-to-saved-hook ()
	(interactive)
	(ibuffer-auto-mode 1)
	(ibuffer-switch-to-saved-filter-groups "nathans"))
  :config
  (setq ibuffer-hidden-filter-groups (list "emacs" "dired" "default")
		ibuffer-default-sorting-mode 'filename/process
		ibuffer-show-empty-filter-groups nil
		ibuffer-saved-filter-groups
        (quote (("nathans"
                 ("dired" (mode . dired-mode))
                 ("planner" (or
                             (name . "^\\*Calendar\\*$")
                             (name . "^diary$")
							 (name . "^appt")
							 (mode . diary-fancy-display-mode)
                             (mode . muse-mode)))
                 ("emacs" (or
						   (mode . emacs-lisp-mode)
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           (name . "^\\*Warnings\\*$")
                           (name . "^\\*Completions\\*$")
                           (name . "^\\*Help\\*$")
						   (name . "^\\*Backtrace\\*$")))
				 ("web" (mode . web-mode))
				 ("shell" (or
						   (mode . eshell-mode)
						   (mode . term-mode)
						   (mode . shell-mode)))
                 ("email" (or
                           (mode . notmuch-hello-mode)
                           (mode . notmuch-show)
                           (mode . notmuch-tree)
                           (mode . notmuch-search)
                           (mode . bbdb-mode)
                           (mode . ebdb-mode)
                           (mode . mail-mode)
                           (name . "^\\.ebdb$"))))))))

(use-package ispell
  :config
  (setq ispell-program-name "aspell"
		ispell-extra-args '("--sug-mode=ultra")))

(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "02:00am"))

(use-package org
  :config
  (setq org-publish-project-alist
		'(("n.d.o"
		   :base-directory "~/Documents/Notes"
		   :recursive t
		   :publishing-directory "~/Documents/Notes/.www/"
		   :publishing-function org-html-publish-to-html))
		org-publish-use-timestamps-flag nil))

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
