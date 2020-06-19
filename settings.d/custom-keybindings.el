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
 ;; Replacement keys
 ;; Replacing the directory list with dired
 ("C-x C-d"         .	ido-dired))
