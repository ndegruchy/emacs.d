(add-to-list 'default-frame-alist '(font . "Cascadia Code-14"))
(setq default-directory "C:/Users/NathanRDeGruchy/"
	  dired-listing-switches "-a -F -l"
	  ls-lisp-emulation nil
	  ls-lisp-dirs-first t
	  ls-lisp-use-insert-directory-program nil)

(use-package files+
  :load (concat user-emacs-directory "site-lisp.d/files+.el"))

(use-package ls-lisp+
  :load (concat user-emacs-directory "site-lisp.d/ls-lisp+.el"))
