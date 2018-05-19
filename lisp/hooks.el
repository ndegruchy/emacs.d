;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks file                       ;;
;; Add mode and function hooks here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-hook 'ndegruchy/c-mode-keybindings)
;; set this in all c-based programming modes
(add-hook 'c-mode-common-hook
          (lambda ()
             (c-set-offset 'case-label '+)))
