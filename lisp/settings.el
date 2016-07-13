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
(set-face-background 'region "light goldenrod")

;; Some default settings
(setq inhibit-startup-message               t
      make-backup-files                     nil
      auto-save-default                     t
      auto-save-interval                    50
      auto-save-timeout                     5
      delete-auto-save-files                t
      case-fold-search                      t
      tooltip-delay                         1
      show-trailing-whitespace              t
      initial-scratch-message               ";; Scratch buffer\n"
      visible-bell                          nil
      ring-bell-function                    (quote ignore)
      sentance-end-double-space             t
      completion-ignore-case                t
      read-file-name-completion-ignore-case t
      initial-major-mode                    (quote text-mode)
      mouse-wheel-progressive-speed         nil
      load-prefer-newer                     t
      save-abbrevs                          t
      confirm-kill-emacs                    'y-or-n-p
      custom-file                           "~/.emacs.d/lisp/custom.el"
      ls-lisp-use-insert-directory-program  nil
      dired-use-ls-dired                    nil
      ring-bell-function                    (quote ignore)
      url-cookie-untrusted-urls             (quote (".*"))
      ispell-personal-dictionary            "~/.emacs.d/etc/personal_dictionary"
      snake-score-file                      "~/.emacs.d/var/snake-scores"
      tetris-score-file                     "~/.emacs.d/var/tetris-scores"
      bubbles-score-file                    "~/.emacs.d/var/bubbles-scores")

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

;; I sometimes am on a laptop, show the battery meter
(display-battery-mode +1)

;; Have IDO/Smex show recent files
(recentf-mode 1)
