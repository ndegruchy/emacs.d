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
 ("C-c 0"           .   ndegruchy/insert-name)
 ("C-c 9"           .   ndegruchy/insert-email)
 ;; Some future bindings
 ("C-x x g"         .   revert-buffer)
 ("C-x x u"         .   rename-uniquely)
 ("C-x x t"         .   toggle-truncate-lines))

;;; (Outline-minor-mode key map)
(define-prefix-command 'cm-map nil "Outline Operation")
										; HIDE
(define-key cm-map "q" 'outline-hide-sublevels)    ; Hide everything but the top-level headings
(define-key cm-map "t" 'outline-hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'outline-hide-other)        ; Hide other branches
(define-key cm-map "c" 'outline-hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'outline-hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'outline-hide-subtree)      ; Hide everything in this entry and sub-entries
										; SHOW
(define-key cm-map "a" 'outline-show-all)          ; Show (expand) everything
(define-key cm-map "e" 'outline-show-entry)        ; Show this heading's body
(define-key cm-map "i" 'outline-show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'outline-show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'outline-show-subtree)      ; Show (expand) everything in this heading & below
										; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
(global-set-key "\M-o" cm-map)
