;; included-packages.el
;; Configure packages distributed with Emacs

(diminish 'auto-revert-mode)

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

;; TODO: Update with SASL and don't save the password
;;       in the source code here...
;; (use-package erc
;;   :config
;;   (require 'erc-join)
;;   (require 'erc-match)
;;   (require 'erc-fill)
;;   (erc-autojoin-mode t)
;;   (erc-match-mode t)

;;   (setq erc-autojoin-channels-alist '(("freenode.net"
;; 									   "#emacs" "#indieweb" "#indieweb-chat" "#fsf"
;; 									   "#fsf-members" "#sr.ht" "#debian"))
;; 		erc-match-keywords '("degruchy" "ndegruchy" "nathan")
;; 		erc-log-insert-log-on-open nil
;; 		erc-log-channels nil
;; 		erc-save-buffer-on-part nil
;; 		erc-timestamp-only-if-changed-flag nil
;;         erc-timestamp-format "%H:%M "
;;         erc-fill-prefix "      "
;;         erc-insert-timestamp-function 'erc-insert-timestamp-left
;; 		erc-prompt-for-nickserv-password t)

;;   (defun irc-maybe ()
;; 	"Ask if we want to connect to IRC"
;; 	(interactive)
;; 	(when (y-or-n-p "Connec to IRC? ")
;; 	  (erc :server "chat.freenode.net" :port 6697
;; 		   :nick "ndegruchy" :full-name "Nathan DeGruchy"))))

(use-package eshell
  :config
  (setq eshell-visual-commands '("less" "tmux" "htop" "top" "bash" "fish" "zsh")
		eshell-visual-subcommands '(("git" "log" "l" "diff" "show"))))

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
