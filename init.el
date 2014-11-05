;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2014-11-04 19:42:38 ndegruchy>

;; Me
(setq user-full-name    "Nathan DeGruchy"
      user-mail-address "nathan@degruchy.org")

;; Setup Emacs Package Management System

(require 'package)
(package-initialize)

;; I like having more choices than the in-built GNU repo
(setq package-archives '(("gnu"         . "http://elpa.gnu.org/packages/")
                         ("marmalade"   . "https://marmalade-repo.org/packages/")
                         ("melpa"       . "http://stable.melpa.org/packages/")
                         ("org"         . "http://orgmode.org/elpa/")
                        ))


;; GUI Features

;; Font
;; My preferred font is Source Code Pro
;;(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
(set-face-attribute 'default nil :family "Source Code Pro" :height 140)
;; Functions take a number. Positive = enabled; Negative = disabled
(global-font-lock-mode +1)

;; Color Scheme
;; Second argument loads the theme without prompting if it's safe
(load-theme 'tango t)

;; Remove some of the window "chrome" like toolbars and scrollbars
;; Functions take a number. Positive = enabled; Negative = disabled
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Fringe
;; I utilize the fringe here to do basically the same-ish thing
;; that scrollbars do, but without the annoying hassle of it being
;; window manager dependant theming
(setq-default indicate-bufffer-boundaries 'left)
(setq-default indicate-empty-lines t)

;; Startup
;; I like a nice, simple startup screen. The welcome screen isn't
;; really that helpful
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Scratch buffer\n")

;; Coding Style

;; Sentences
(setq sentence-end-double-space t)

;; Line numbers
;; Turn off line numbers
;; Functions take a number. Positive = enabled; Negative = disabled
(global-linum-mode -1)

;; Indentation
(setq-default indent-tabs-mode ())
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; Scrolling
;; Make scrolling a little bit less janky
(setq redisplay-dont-pause t
    scroll-margin 1
    scroll-step 1
    scroll-conservatively 10000
    scroll-preserve-screen-position t)

;; Searching
;; Case insensitive searching, ain't nobody got time for
;; case sensitivity!
(setq completion-ignore-case                t
      read-file-name-completion-ignore-case t)

;; Key Bindings
;; Quick align-regexp bind, as well as an easy
;; insert date one.
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-c d")  'insert-date)

;; ================= Packages

;; Evil
(require 'evil)
(require 'evil-leader)
(require 'evil-numbers)
(require 'evil-surround)
(evil-mode +1)
(setq evil-default-cursor '(t))
(global-evil-surround-mode)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "k"    'kill-buffer)
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

;; JavaScript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; ORG Mode
(require 'org-mouse)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-src-fontify-natively ())
(setq org-src-tab-acts-natively t)

;; Markdown
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

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

(defun kill-all-dired-buffers ()
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

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; MacOS X Fixes
;; Fix an issue on Mac where you start from a GUI and
;; Emacs fails to pull in the right PATH variable
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Before saving
(add-hook 'before-save-hook 'time-stamp)
