;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-z" "\C-xc" "\C-x\C-z" "\C-x\C-d" "\M-o" "\M-z" "\M-Z" "\C-x\C-r"))
  (global-unset-key key))

(bind-keys*
 ;; Personal map for commands
 ("C-c h"			.	split-window-vertically)
 ("C-c v"			.	split-window-horizontally)
 ("C-S-z"			.	bury-buffer)
 ("<f5>"            .	toggle-truncate-lines)
 ;; Custom functions
 ("C-c R"			.	ndegruchy/rename-file-and-buffer)
 ("S-<return>"		.	ndegruchy/smart-open-line)
 ("C-S-<return>"	.	ndegruchy/smart-open-line-above)
 ("C-c m"			.	ndegruchy/select-current-line-dwim)
 ("C-c d"			.	ndegruchy/insert-date)
 ("C-c a"			.	ndegruchy/duplicate-line)
 ("M-<up>"          .	ndegruchy/move-line-up)
 ("M-<down>"        .	ndegruchy/move-line-down)
 ("M-Q"             .	unfill-paragraph)
 ;; ("C-c q"           .   endless/ispell-word-then-abbrev)
 ("C-c 0"           .   ndegruchy/insert-name)
 ("C-c 9"           .   ndegruchy/insert-email)
 ;; Some future bindings
 ("C-x x g"         .   revert-buffer)
 ("C-x x u"         .   rename-uniquely)
 ("C-x x t"         .   toggle-truncate-lines))
