;;; Org mode skeletons

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
