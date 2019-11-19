;; included-packages.el
;; Configure packages distributed with Emacs

(use-package dired
  :bind (:map dired-mode-map
	      ;; Reuse the same dired window
	      ("RET" . dired-find-alternate-file)
	      ("^"   . (lambda()
			 (interactive)
			 (find-alternate-file "..")))
	      ;; Use 'open' to open the file with the user's choice
	      ("E"   . ndegruchy/open-in-external-app)
	      ("; d" . dired-get-size)
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
