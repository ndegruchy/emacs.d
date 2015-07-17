;; Nathan's Emacs File
;; Now with less Cider
;; Time-stamp: <2015-07-16 21:41:39 ndegruchy>

;; Me
(setq user-full-name    "Nathan DeGruchy"
      user-mail-address "nathan@degruchy.org")

;; Window Geometry
(add-to-list 'default-frame-alist '(width  . 80))
(add-to-list 'default-frame-alist '(height . 24))

;; Quiet, please
(custom-set-variables '(ring-bell-function 'ignore))

;; Some default settings
(setq inhibit-startup-message               t
      make-backup-files                     nil
      auto-save-default                     t
      auto-save-interval                    50
      auto-save-timeout                     5
      delete-auto-save-files                t
      case-fold-search                      t
      tooltip-delay                         1
      show-trailing-whitespace              1
      initial-scratch-message               ";; Scratch buffer\n"
      visible-bell                          nil
      sentance-end-double-space             t
      completion-ignore-case                t
      read-file-name-completion-ignore-case t
      initial-major-mode                    (quote text-mode))

;; GUI Features

;; Font
;; My preferred font is Source Code Pro
(set-face-attribute 'default nil :family "Source Code Pro" :height 140)
(set-frame-font "Source Code Pro-14")
(global-font-lock-mode +1)

;; Delete/Overwrite Selection
(delete-selection-mode t)

;; Remove some of the window "chrome" like toolbars and scrollbars
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Coding Style

;; Sentences
(setq sentence-end-double-space t)

;; Line numbers
(global-linum-mode -1)

;; Indentation
(setq-default indent-tabs-mode ())
(setq-default tab-width 4)
(setq-default tab-always-indent t)
(electric-indent-mode 1)
(global-set-key (kbd "<RET>") 'newline-and-indent)

;; Yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't warn
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Setup Emacs Package Management System

;; Package management on
(require 'package)

;; Make MELPA the default and only
;; TODO: Add orgmode?
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;; If we're running less than emacs 24, load the gnu archives as well
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Start package system
(package-initialize)

;; Refresh package contents, so that we don't get the errors of things
;; not being available. This only happens when we haven't loaded the
;; package list before
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Start `use-package' for later
(require 'use-package)

;; Key Bindings
(global-set-key (kbd "C-c \\") 'align-regexp)
(global-set-key (kbd "C-c d")  'insert-date)
(global-set-key (kbd "C-c b")  'bs-show)
(global-set-key (kbd "C-c j")  'join-line)
(global-set-key (kbd "C-c k")  'kill-whole-line)
(global-set-key (kbd "C-c ;") 'endless/comment-line)
(global-set-key (kbd "C-c <up>") 'text-scale-increase)
(global-set-key (kbd "C-c <down>") 'text-scale-decrease)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "C-c l") 'ndegruchy-select-current-line)
(global-set-key (kbd "C-c q") 'ndegruchy-select-text-in-quote)
(global-set-key (kbd "C-c w") 'mark-word)
(global-set-key (kbd "C-c C-n") 'xah-new-empty-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-d") 'ndegruchy-duplicate-line)
;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; ================= Packages

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package async)
(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t))
(use-package tex-site
  :ensure auctex)
(use-package browse-kill-ring
  :ensure t)
(use-package caps-lock
  :ensure t)
(use-package coffee-mode
  :ensure t)
(use-package csv-mode
  :ensure t)
(use-package cl
  :ensure t)
(use-package dash
  :ensure t)
(use-package abbrev
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))
(use-package git-commit
  :ensure t)
(use-package goto-chg
  :ensure t)
(use-package graphviz-dot-mode
  :ensure t)
(use-package haml-mode
  :ensure t)
(use-package hydra
  :ensure t)
(use-package json-mode
  :ensure t)
(use-package let-alist
  :ensure t)
(use-package magit
  :ensure t
  :bind ("C-c g s" . magit-status))
(use-package magit-find-file
  :ensure t)
(use-package magit-gitflow
  :ensure t)
(use-package magit-popup
  :ensure t)
(use-package makey
  :ensure t)
;; (use-package pandoc-mode
;;   :ensure t)
(use-package paredit
  :ensure t)
(use-package php-mode
  :ensure t)
(use-package pkg-info
  :ensure t)
(use-package s
  :ensure t)
(use-package sass-mode
  :ensure t)
(use-package scss-mode
  :ensure t)
(use-package smartparens
  :ensure t)
(use-package systemd
  :ensure t)
(use-package with-editor
  :ensure t)
(use-package yaml-mode
  :ensure t)

;; Since I use FISH as my preferred shell, I have to
;; have Emacs parse the $PATH in a different way
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; Zap to char -- like vim's `d t <foo>'
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Electric Pair Mode
(electric-pair-mode 1)

;; YaSnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; Hungry like the woooolllffff
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;; IDO
(use-package ido
  :ensure t
  :config
  (ido-mode +1)
  (setq ido-enable-flex-matching +1)
  (setq ido-everywhere +1)
  (setq ido-file-extensions-order '(".org" ".html" ".php" ".tex" ".el" ".js" ".coffee")))

;; Emmet
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'sgml-mode-hook 'toggle-truncate-lines)
  (add-hook 'web-mode-hook  'toggle-truncate-lines)
  (add-hook 'php-mode-hook  'toggle-truncate-lines))


;; JavaScript
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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
(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))


;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Rainbow Mode
(use-package rainbow-mode
  :ensure t
  :config
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))

;; Rainbow Delimiter mode
(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :config
  (add-hook 'lisp-mode       'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode 'rainbow-delimiters-mode))

;; Smex
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

;; Expand Region
(use-package expand-region
  :ensure t
  :bind ("M-+" . er/expand-region))

;; Dired
;;(define-key dired-mode-map (kbd "Z") 'dired-get-size)

;; Discover
(use-package discover
  :ensure t
  :config
  (global-discover-mode 1))

;; Show battery percentage
(display-battery-mode +1)

;; Recent Files
(recentf-mode 1)
(global-set-key (kbd "C-c f") 'ido-choose-from-recentf)

;; Custom Functions

;; Dired DU Function
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

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

(defun ido-choose-from-recentf ()
  "Use ido to select a recently visited file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

;; Create parent folder(s) when visiting a non-existant file
(defun my-create-non-existent-directory ()
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p parent-directory))
                   (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
          (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;; Open file in preferred app
(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

Version 2015-01-26
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'"
  (interactive)
  (let* (
         (ξfile-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (ξdo-it-p (if (<= (length ξfile-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))

    (when ξdo-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (fPath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) ξfile-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  ξfile-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) ξfile-list))))))

;; Open in file manager
(defun xah-open-in-desktop ()
  "Show current file in desktop (OS's file manager)."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    ) ))

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

(defun endless/comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."
  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (goto-char (line-end-position n)))
  (forward-line 1)
  (back-to-indentation))

(defun my-asciify-string (string)
"Convert STRING to ASCII string.
For example:
“passé” becomes “passe”"
;; Code originally by Teemu Likonen
  (with-temp-buffer
    (insert string)
    (call-process-region (point-min) (point-max) "iconv" t t nil "--to-code=ASCII//TRANSLIT")
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ndegruchy-title-case-region-or-line (φp1 φp2)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, φp1 φp2 are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2015-04-08"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           ξp1
           ξp2
           (ξskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward ξskipChars (line-beginning-position))
         (setq ξp1 (point))
         (skip-chars-forward ξskipChars (line-end-position))
         (setq ξp2 (point)))
       (list ξp1 ξp2))))
  (let* (
         (ξstrPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ]))
    (save-restriction
      (narrow-to-region φp1 φp2)
      (upcase-initials-region (point-min) (point-max))
      (let ((case-fold-search nil))
        (mapc
         (lambda (ξx)
           (goto-char (point-min))
           (while
               (search-forward (aref ξx 0) nil t)
             (replace-match (aref ξx 1) 'FIXEDCASE 'LITERAL)))
         ξstrPairs)))))

(defun ndegruchy-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters are paired characters: () [] {} <> «» ‹› “” ‘’ 「」 【】《》〈〉〔〕（）, including \"\".

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
version 2015-02-07
"
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "^<>(“{[«‹「【《〈〔（\"‘")
    (setq p1 (point))
    (skip-chars-forward "^<>)”}]»›」】》〉〕）\"’")
    (setq p2 (point))
    (set-mark p1)))

(defun ndegruchy-select-current-line ()
  "Select current line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-02-07
"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun turn-off-mouse (&optional frame)
  (interactive)
  (shell-command "xinput --disable \"AlpsPS\/2 ALPS DualPoint TouchPad\""))

(defun turn-on-mouse (&optional frame)
  (interactive)
  (shell-command "xinput --enable \"AlpsPS\/2 ALPS DualPoint TouchPad\""))

(defun unix-file ()
      "Change the current buffer to Latin 1 with Unix line-ends."
      (interactive)
      (set-buffer-file-coding-system 'iso-latin-1-unix t))

(defun dos-file ()
      "Change the current buffer to Latin 1 with DOS line-ends."
      (interactive)
      (set-buffer-file-coding-system 'iso-latin-1-dos t))

(defun mac-file ()
      "Change the current buffer to Latin 1 with Mac line-ends."
      (interactive)
      (set-buffer-file-coding-system 'iso-latin-1-mac t))

(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2015-06-12"
  (interactive)
  (let ((ξbuf (generate-new-buffer "untitled")))
    (switch-to-buffer ξbuf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(defun ndegruchy-duplicate-line()
  "Taken from `http://stackoverflow.com/a/88828'"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; (add-hook 'focus-in-hook #'turn-off-mouse)
;; (add-hook 'focus-out-hook #'turn-on-mouse)
;; (add-hook 'delete-frame-functions #'turn-on-mouse)

;; Before saving
(add-hook 'before-save-hook 'time-stamp)
