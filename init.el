;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2014-12-23 12:33:23 ndegruchy>

;; Me
(setq user-full-name    "Nathan DeGruchy"
      user-mail-address "nathan@degruchy.org")

;;(when window-system (set-frame-size (selected-frame) 80 24))
(add-to-list 'default-frame-alist '(width  . 80))
(add-to-list 'default-frame-alist '(height . 24))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Setup Emacs Package Management System

(require 'package)
(package-initialize)

;; I like having more choices than the in-built GNU repo
(setq package-archives '(("gnu"         . "http://elpa.gnu.org/packages/")
                         ("marmalade"   . "https://marmalade-repo.org/packages/")
                         ("melpa"       . "http://stable.melpa.org/packages/")
                         ("org"         . "http://orgmode.org/elpa/")
                        ))


;; Since I use FISH as my preferred shell, I have to
;; have Emacs parse the $PATH in a different way
;;(exec-path-from-shell-initialize)

;; GUI Features

;; Font
;; My preferred font is Source Code Pro
;;(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
(set-face-attribute 'default nil :family "Source Code Pro" :height 140)
;; Functions take a number. Positive = enabled; Negative = disabled
(global-font-lock-mode +1)

;; Color Scheme
;; Second argument loads the theme without prompting if it's safe
;; (load-theme 'tango t)
(load-theme 'jazz t)

;; Remove some of the window "chrome" like toolbars and scrollbars
;; Functions take a number. Positive = enabled; Negative = disabled
(tool-bar-mode -1)
(scroll-bar-mode -1)

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
(setq-default tab-always-indent t)
(electric-indent-mode 1)

;; Searching
;; Case insensitive searching, ain't nobody got time for
;; case sensitivity!
(setq completion-ignore-case                t
      read-file-name-completion-ignore-case t)

;; Key Bindings
;; Quick align-regexp bind, as well as an easy
;; insert date one.
(global-set-key (kbd "C-c \\") 'align-regexp)
(global-set-key (kbd "C-c d")  'insert-date)
(global-set-key (kbd "C-c b")  'bs-show)
(global-set-key (kbd "C-c gs") 'magit-status)
(global-set-key (kbd "C-c j")  'join-line)
(global-set-key (kbd "C-c k")  'kill-whole-line)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))

;; ================= Packages

;; Hungry like the woooolllffff
(unless (fboundp 'hungry-delete-mode)
  (package-install 'hungry-delete))

(require 'hungry-delete)
(global-hungry-delete-mode)

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

;; Org Mode
(require 'org-mouse)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'electric-indent-mode)
(add-hook 'org-mode-hook 'org-display-inline-images)
(setq org-src-fontify-natively ())
(setq org-src-tab-acts-natively t)
(setq org-support-shift-select t)

;; Markdown
(add-hook 'markdown-mode-hook 'flyspell-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Rainbow Mode
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))

;; Rainbow Delimiter mode
(dolist (hook '(lisp-mode))
  (add-hook hook 'rainbow-delimiters-mode))

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Expand Region
(require 'expand-region)
;; (global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "M-+") 'er/expand-region)

(display-battery-mode +1)

;; Custom Functions

;; after deleting a tag, indent properly
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

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

(defun kill-whole-line nil
  "kills the entire line on which the cursor is located, and
places the cursor as close to its previous position as possible."
  (interactive)
  (progn
    (let ((y (current-column))
          (a (progn (beginning-of-line) (point)))
          (b (progn (forward-line 1) (point))))
      (kill-region a b)
      (move-to-column y))))

(defun open-line-below ()
  "Inserts a line below the current line, moving the cursor to
that line and setting the indent properly"
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
   "Inserts a line above the current line, moving the cursor to
that line and setting the indent properly"
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;; Ugly hack to fix epa-list-keys
(defun epg--list-keys-1 (context name mode)
  (let ((args (append (if (epg-context-home-directory context)
              (list "--homedir"
                (epg-context-home-directory context)))
              '("--with-colons" "--no-greeting" "--batch"
            "--with-fingerprint" "--with-fingerprint")
              (unless (eq (epg-context-protocol context) 'CMS)
            '("--fixed-list-mode"))))
    (list-keys-option (if (memq mode '(t secret))
                  "--list-secret-keys"
                (if (memq mode '(nil public))
                "--list-keys"
                  "--list-sigs")))
    (coding-system-for-read 'binary)
    keys string field index)
    (if name
    (progn
      (unless (listp name)
        (setq name (list name)))
      (while name
        (setq args (append args (list list-keys-option (car name)))
          name (cdr name))))
      (setq args (append args (list list-keys-option))))
    (with-temp-buffer
      (apply #'call-process
         (epg-context-program context)
         nil (list t nil) nil args)
      (goto-char (point-min))
      (while (re-search-forward "^[a-z][a-z][a-z]:.*" nil t)
    (setq keys (cons (make-vector 15 nil) keys)
          string (match-string 0)
          index 0
          field 0)
    (while (and (< field (length (car keys)))
            (eq index
            (string-match "\\([^:]+\\)?:" string index)))
      (setq index (match-end 0))
      (aset (car keys) field (match-string 1 string))
      (setq field (1+ field))))
      (nreverse keys))))

;; Create parent folder(s) when visiting a non-existant file
(defun my-create-non-existent-directory ()
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p parent-directory))
                   (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
          (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;; Autocorrect word

(define-key ctl-x-map "\C-i" 'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

(setq save-abbrevs t)
(setq-default abbrev-mode t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Before saving
(add-hook 'before-save-hook 'time-stamp)
