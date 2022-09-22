;;; rec.el --- A skeleton for inserting rec entries  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: local, data, processes, convenience

(define-skeleton ndegruchy/skeleton-rec-new-contact
	"Inserts a skeletal contact card record for a rec-mode database"
	nil
	'(setq gn (skeleton-read "Given Name: "))
	'(setq fn (skeleton-read "Family Name: "))
	'(setq ea (skeleton-read "Email: "))
	'(setq ph (skeleton-read "Phone: "))
	;; Should I just ask for these up front? Seems like an obvious choice
	"Given_Name: " gn "\n"
	"Family_Name: " fn " \n"
	"Street_Address: " _ " \n"
	"Email_Address: " ea " \n"
	"Phone: " ph " \n")

(define-skeleton ndegruchy/skeleton-rec-new-file
	"Inserts a skeleton for new recutils file"
	nil
	;; tbd
	"# -*- mode: rec -*-\n"
	"\n"
	"$rec: " _ "\n"
	"%doc: " _ "\n"
	)
