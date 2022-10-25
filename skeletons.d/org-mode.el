;;; org-mode.el --- A collection org-mode skeletons  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: local, data, processes, convenience

(define-skeleton ndegruchy/skeleton-org-new-file
	"Inserts some basic 'front matter' into an org file"
	"Title: "
	"#+TITLE: " str "\n"
	"#+AUTHOR: " (user-full-name) "\n"
	"#+DATE: " (format-time-string "%Y-%m-%d") "\n"
	"#+OPTIONS: toc:nil num:nil\n"
	"#+Time-stamp: <>\n"
	"\n"
	_)

;; Local Variables:
;; truncate-lines: t
;; End:
