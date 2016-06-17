;;;;;;;;;;;
;; Hooks ;;
;;;;;;;;;;;

;; Time Stamping
(add-hook 'before-save-hook 'time-stamp)

;; Flyspell mode in text-mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; Some convenient vars for eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "pager" "cat")
            (setenv "editor" "emacsclient")))
