;;; modes.el --- A collection of modes created by me (more likely stolen from somewhere else...)  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: docs, help, tools, internal, convenience

(defvar-local hide-cursor--original nil)

(define-minor-mode ndegruchy/hide-cursor-mode
	"Hide or show the cursor.

When the cursor is hidden `scroll-lock-mode' is enabled, so that
the buffer works like a pager.

https://karthinks.com/software/more-less-emacs/"
	:global nil
	:lighter "H"
	(if ndegruchy/hide-cursor-mode
		(progn
			(scroll-lock-mode 1)
			(read-only-mode 1)
			(setq-local hide-cursor--original
                cursor-type)
			(setq-local cursor-type nil))
		(scroll-lock-mode -1)
		(read-only-mode -1)
		(setq-local cursor-type (or hide-cursor--original
									t))))
