;;; latex.el --- A collection latex skeletons  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: local, data, processes, convenience

;; LaTeX (TeX) Skeletons. Ball-gag not included.
(define-skeleton ndegruchy/latex-base
	"Default LaTeX/TeX file contents"
	"Title: "
	"\\documentclass[11pt]{article}\n"
	"\\title{" str | " ENTER TITLE HERE " "}\n"
	"\\begin{document}\n"
	"\\maketitle\n"
	"\n" _ "\n\n"
	"\\end{document}")
