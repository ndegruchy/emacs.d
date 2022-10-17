;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-z" "\C-xc" "\C-x\C-z" "\C-x\C-d" "\M-o" "\M-z" "\M-Z" "\C-x\C-r"))
	(global-unset-key key))

;; Personal map for existing commands
(global-set-key (kbd "C-c h")	'split-window-vertically)
(global-set-key (kbd "C-c v")	'split-window-horizontally)
(global-set-key (kbd "<f5>")	'toggle-truncate-lines)
(global-set-key (kbd "M-/")		'hippie-expand)
(global-set-key (kbd "M-z")		'zap-to-char)
(global-set-key (kbd "M-Z")		'zap-up-to-char)
(global-set-key (kbd "C-x k")	'kill-this-buffer)

;; Custom functions
(global-set-key (kbd "C-c m")	'ndegruchy/select-current-line-dwim)
(global-set-key (kbd "C-c d")	'ndegruchy/insert-date)
(global-set-key (kbd "C-c a")	'ndegruchy/duplicate-line)
(global-set-key (kbd "C-c M")	'ndegruchy/make)
(global-set-key (kbd "M-<up>")	'ndegruchy/move-line-up)
(global-set-key (kbd "M-<down>") 'ndegruchy/move-line-down)
(define-key minibuffer-local-filename-completion-map (kbd "C-l") 'ndegruchy/up-directory)
