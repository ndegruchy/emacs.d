;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-z" "\C-x\C-z" "\C-x\C-d" "\M-o" "\M-z" "\M-Z" "\C-x\C-r"))
  (global-unset-key key))

(bind-keys*
 ;; Personal map for commands
 ("C-c h"			.	split-window-vertically)
 ("C-c v"			.	split-window-horizontally)
 ("C-S-z"			.	bury-buffer)
 ("<f5>"            .	toggle-truncate-lines)
 ("s-<backspace>"   .   kill-this-buffer)
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
 ("C-c i"           .   ndegruchy/insert-file-name)
 ("C-c q"           .   endless/ispell-word-then-abbrev))
