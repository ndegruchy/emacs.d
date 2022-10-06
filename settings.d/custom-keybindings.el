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
	("C-c C"			.	calendar)
	("<f5>"            	.	toggle-truncate-lines)
	("M-/"             	.   hippie-expand)
	("M-z"				.	zap-to-char)
	("M-Z"				.	zap-up-to-char)
	("C-x k"			.   kill-this-buffer)
	("M-#"				.	dictionary-lookup-definition)
	;; Custom functions
	("C-c R"			.	ndegruchy/rename-file-and-buffer)
	("S-<return>"		.	ndegruchy/smart-open-line)
	("C-S-<return>"		.	ndegruchy/smart-open-line-above)
	("C-c m"			.	ndegruchy/select-current-line-dwim)
	("C-c d"			.	ndegruchy/insert-date)
	("C-c a"			.	ndegruchy/duplicate-line)
	("M-<up>"			.	ndegruchy/move-line-up)
	("M-<down>"        	.	ndegruchy/move-line-down)
	("C-c l"			.   ndegruchy/irc)
	("C-c M"			.	ndegruchy/make)
	("C-c w a"			.	ndegruchy/skeleton-web-article)
	("C-c w g"			.	ndegruchy/skeleton-web-gallery)
	("C-c w p"			.	ndegruchy/skeleton-web-picture)
	("C-c w t"			.	ndegruchy/skeleton-web-atom-entry)
	("C-c w n"			.	ndegruchy/skeleton-rec-new-contact)
	("<f7>"				.	ndegruchy/hide-cursor-mode)
	:map minibuffer-local-filename-completion-map
	;; map to simulate Helm's excellent 'up-directory' mapping
	("C-l"				.	ndegruchy/up-directory))
