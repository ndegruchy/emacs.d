;; General Key Bindings
(global-set-key (kbd "C-c \\") 'align-regexp)
(global-set-key (kbd "C-c d")  'ndegruchy/insert-date)
(global-set-key (kbd "C-c b")  'bs-show)
(global-set-key (kbd "C-c ;") 'ndegruchy/comment-line)
(global-set-key (kbd "C-c <up>") 'text-scale-increase)
(global-set-key (kbd "C-c <down>") 'text-scale-decrease)
(global-set-key (kbd "C-c C-n") 'ndegruchy/new-empty-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-d") 'ndegruchy/duplicate-line)
;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

(global-set-key (kbd "C-c f") 'ndegruchy/ido-choose-from-recentf)
