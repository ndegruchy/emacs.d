;; Nathan's Emacs File
;; Now with less Cider

;; MacOS X Fixes
;; Fix an issue on Mac where you start from a GUI and
;; Emacs fails to pull in the right PATH variable
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

;; GUI Features

;; Font
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))

;; Color Scheme
(load-theme 'tango-dark)

;; Chrome
;; (when (window-system)
	(tool-bar-mode -1)
	(scroll-bar-mode -1)
	(set-frame-height (selected-frame) 30)
	(set-frame-width  (selected-frame) 80)
;; )

;; Fringe
(setq-default indicate-bufffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;; Startup
(setq inhibit-startup-message +1)
(setq initial-scratch-message ";; Scratch buffer\n")

;; Coding Style

;; Line numbers
(global-linum-mode -1)

;; Indentation
(setq-default indent-tabs-mode -1)
(setq-default tab-width 4)

;; Scrolling
(setq redisplay-dont-pause t
    scroll-margin 1
    scroll-step 1
    scroll-conservatively 10000
    scroll-preserve-screen-position 1)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Searching
(setq completion-ignore-case t
	read-file-name-completion-ignore-case t)

;; Key Bindings

;; Align
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-c d")  'insert-date)


;; Packages

(require 'package)
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
						 ("org" . "http://orgmode.org/elpa/")
						 )
	  )

;; Evil
(require 'evil)
(require 'evil-leader)
(require 'evil-numbers)
(require 'evil-surround)
(evil-mode 1)
(global-evil-surround-mode)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "k"    'kill-buffer)
(evil-leader/set-key "bd"   'kill-buffer)
(evil-leader/set-key "gs"   'magit-status)
(evil-leader/set-key "cl"   'org-store-link)
(evil-leader/set-key "cc"   'org-capture)
(evil-leader/set-key "ca"   'org-agenda)
(evil-leader/set-key "cb"   'org-iswitchb)
(evil-leader/set-key "b"    'bs-show)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
(add-to-list 'evil-emacs-state-modes 'dired-mode)
(add-to-list 'evil-emacs-state-modes 'reb-mode)
(add-to-list 'evil-emacs-state-modes 'wdired-mode)
(add-to-list 'evil-emacs-state-modes 'eshell-mode)
(add-to-list 'evil-emacs-state-modes 'bs-mode)
(evil-set-initial-state 'eshell-mode 'emacs)
;; Wow. Okay.
;; Found: https://stackoverflow.com/questions/8204316/cant-change-cursor-color-in-emacsclient
(setq evil-default-cursor +1)
(set-cursor-color "white")
(set-mouse-color "white")
;; (setq evil-default-cursor (quote (t "white")))
;; '(evil-default-cursor (quote (t "white")))

;; IDO
(require 'ido)
(ido-mode +1)
(setq ido-enable-flex-matching +1)
(setq ido-everywhere +1)
(setq ido-file-extensions-order '(".org" ".html" ".php" ".tex" ".el" ".js" ".coffee"))

;; Emmet
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'sgml-mode-hook 'electric-pair-mode)
(add-hook 'web-mode-hook  'electric-pair-mode)
(add-hook 'css-mode-hook  'electric-pair-mode)
(add-hook 'json-mode-hook 'electric-pair-mode)
(add-hook 'sgml-mode-hook 'toggle-truncate-lines)
(add-hook 'web-mode-hook  'toggle-truncate-lines)
(add-hook 'php-mode-hook  'toggle-truncate-lines)

;; ORG Mode
(require 'org-mouse)
(require 'org-agenda)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-agenda-span 14)

;; Markdown
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Custom Functions

(defun insert-date (format)
  "Wrapper around format-time-string"
  (interactive "MFormat: ")
  (insert (format-time-string format)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a
  buffer.  Including indent-buffer, which should not be called
  automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun kill-all-dired-buffers()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
	(let((count 0))
	  (dolist(buffer (buffer-list))
		(set-buffer buffer)
		(when (equal major-mode 'dired-mode)
		  (setq count (1+ count))
		  (kill-buffer buffer)))
	  (message "Killed %i dired buffer(s)." count ))))

(custom-set-variables
  '(auto-save-file-name-transforms ' ((".*"
  "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups"))))
(make-directory "~/.emacs.d/autosaves" t)
