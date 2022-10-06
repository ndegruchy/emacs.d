;;; Org mode skeletons

(define-skeleton ndegruchy/skeleton-org-new-file
	"Inserts some basic 'front matter' into an org file"
	"Title: "
	"#+TITLE: " str
	"#+AUTHOR: " (user-full-name)
	"#+DATE: " (format-time-string "%Y-%m-%d")
	"#+OPTIONS: toc:nil num:nil"
	"#+Time-stamp: <>")
