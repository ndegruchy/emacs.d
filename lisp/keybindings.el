;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c \\")     'align-regexp)
(global-set-key (kbd "M-/")        'hippie-expand)
(global-set-key (kbd "C-c b")      'bs-show)
(global-set-key (kbd "C-c <up>")   'text-scale-increase)
(global-set-key (kbd "C-c <down>") 'text-scale-decrease)
;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Custom function binds
(global-set-key (kbd "C-c ;")          #'ndegruchy/comment-line)
(global-set-key (kbd "C-c C-n")        #'ndegruchy/new-empty-buffer)
(global-set-key (kbd "C-c C-d")        #'ndegruchy/duplicate-line)
(global-set-key (kbd "C-c o")          #'ndegruchy/open-line-below)
(global-set-key (kbd "C-c O")          #'ndegruchy/open-line-above)
(global-set-key (kbd "C-c k")          #'ndegruchy/kill-whole-line)
(global-set-key (kbd "C-c m")          #'ndegruchy/select-current-line)
(global-set-key (kbd "C-c w")          #'ndegruchy/select-current-word)
(global-set-key (kbd "C-c d")          #'ndegruchy/insert-date)
(global-set-key (kbd "C-c f")          #'ndegruchy/ido-choose-from-recentf)
(global-set-key [remap fill-paragraph] #'ndegruchy/fill-or-unfill)
(global-set-key (kbd "C-x C-i")        #'ndegruchy/ispell-word-then-abbrev)

;; Dired mode binding
(define-key dired-mode-map "Y"         #'ndegruchy/dired-rsync)
