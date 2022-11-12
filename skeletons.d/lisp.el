;;; lisp.el --- A series of skeletons for elisp stuff  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: abbrev, convenience, languages, lisp, local

(define-skeleton ndegruchy/skeleton-elisp-external-package
  "Inserts a skeleton for installing a missing external package"
  "Package name: "
  "(unless (package-installed-p '" str ")\n"
  > "(package-refresh-contents)\n"
  > "(package-install '" str "))\n")
