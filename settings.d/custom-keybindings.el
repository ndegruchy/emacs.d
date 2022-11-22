;;; custom-keybindings --- A collection of site-local keybindings for my emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: local, data, processes, convenience

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
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Custom functions
(global-set-key (kbd "C-c m")	'ndegruchy/select-current-line-dwim)
(global-set-key (kbd "C-c d")	'ndegruchy/insert-date)
(global-set-key (kbd "C-c a")	'ndegruchy/duplicate-line)
(global-set-key (kbd "C-c M")	'ndegruchy/make)
(global-set-key (kbd "M-<up>")	'ndegruchy/move-line-up)
(global-set-key (kbd "M-<down>") 'ndegruchy/move-line-down)
(global-set-key (kbd "C-c o")	'ndegruchy/occur-dwim)
(global-set-key (kbd "<f6>")	'ndegruchy/revert-buffer-noconfirm)
(global-set-key (kbd "<f9>")	'ndegruchy/forward-or-backward-sexp)
(define-key minibuffer-local-filename-completion-map (kbd "C-l") 'ndegruchy/up-directory)

;; Local Variables:
;; truncate-lines: t
;; End:
