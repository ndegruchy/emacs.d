;;; rec.el --- A skeleton for inserting rec entries  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: local, data, processes, convenience

(define-skeleton ndegruchy/skeleton-rec-new-contact
	"Inserts a skeletal contact card record for a rec-mode database"
	nil
	;; Should I just ask for these up front? Seems like an obvious choice
	"Given_Name: " _ "\n"
	"Family_Name: " _ " \n"
	"Street_Address: " _ " \n"
	"Email_Address: " _ " \n"
	"Phone: " _ " \n")

(define-skeleton ndegruchy/skeleton-rec-new-file
	"Inserts a skeleton for new recutils file"
	nil
	;; tbd
	)
