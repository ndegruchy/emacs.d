;;;;;;;;;;;;;;;;;;;
;; Settings File ;;
;;;;;;;;;;;;;;;;;;;

;; Me
(setq user-full-name    "Nathan DeGruchy"
      user-mail-address "nathan@degruchy.org")

;; Window Geometry
(add-to-list 'default-frame-alist '(width  . 100))
(add-to-list 'default-frame-alist '(height . 34))

;; Undo some of the default theme settings that are
;; defined by GTK, making things across platforms
;; slightly more unified
;; (set-face-background 'region "light goldenrod")

;; Some default settings
(setq inhibit-startup-message                t
      initial-scratch-message                ";; Scratch buffer\n"
      initial-major-mode                     (quote text-mode)
      ring-bell-function                     (quote ignore)
      confirm-kill-emacs                     'y-or-n-p
      save-abbrevs                           t
      load-prefer-newer                      t
      mouse-wheel-progressive-speed          nil
      read-file-name-complection-ignore-case t
      completion-ignore-case                 t
      tooltip-delay                          1
      show-trailing-whitespace               t
      view-diary-entries-initially           t
      mark-diary-entries-in-calendar         t
      number-of-diary-entries                7
      inhibit-x-resources                    t)

;; Diary
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(setq calendar-latitude                      30.33
      calendar-longitude                     -81.65
      calendar-location-name                 "Jacksonville, FL")

;; Dired
(setq dired-use-ls-dired                     nil
      ls-lisp-use-insert-directory-program   nil)

;; Backup file configuration
(setq backup-directory-alist                 '(("." . "~/.emacs.d/backup"))
      make-backup-files                      t
      version-control                        t
      delete-old-version                     t
      kept-new-versions                      20
      kept-old-versions                      5
      auto-save-default                      t
      auto-save-interval                     50
      auto-save-timeout                      5
      delete-auto-save-files                 t)

;; Various config files
(setq snake-score-file                       "~/.emacs.d/var/snake-scores"
      tetris-score-file                      "~/.emacs.d/var/tetris-scores"
      bubbles-score-file                     "~/.emacs.d/var/bubbles-scores"
      ispell-personal-dictionary             "~/.emacs.d/etc/personal_dictionary"
      custom-file                            "~/.emacs.d/lisp/custom.el"
      diary-file                             "~/.emacs.d/var/diary")

;; URL cookies
(setq url-cookie-untrusted-urls              (quote (".*")))

(setq-default abbrev-mode t)

;; Ignore some files and extensions
(add-to-list 'completion-ignored-extensions ".sass-cache/")
(add-to-list 'completion-ignored-extensions ".idea/")
(add-to-list 'completion-ignored-extensions ".git/")
(add-to-list 'completion-ignored-extensions "node-modules/")

;; Find file creates non-existent folders
(add-to-list 'find-file-not-found-functions #'ndegruchy/my-create-non-existent-directory)

;; GUI Features

;; Font
;; My preferred font is Source Code Pro
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
(global-font-lock-mode +1)

;; Delete/Overwrite Selection
(delete-selection-mode t)

;; Remove some of the window "chrome" like toolbars and scrollbars
(tool-bar-mode    -1)
(menu-bar-mode    -1)
(scroll-bar-mode  -1)

;; Coding Style

;; Line numbers
(global-linum-mode -1)

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              tab-always-indent t)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(electric-indent-mode 1)
(global-set-key (kbd "<RET>") 'newline-and-indent)

;; Yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't warn
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable automatically pairing of parens, brackets, etc
(electric-pair-mode 1)
(show-paren-mode 1)

;; I sometimes am on a laptop, show the battery meter
(display-battery-mode +1)

;; Have IDO/Smex show recent files
(recentf-mode 1)

;; Time Stamping
(add-hook 'before-save-hook 'time-stamp)
