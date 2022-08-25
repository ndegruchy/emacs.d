;;; news-feeds.el --- A set of configurations and articles for NewsTicker  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: comm, news, hypermedia

(use-package newsticker
  :ensure nil
  :bind (("C-c n l" . newsticker-show-news)
		 ("C-c n s" . newsticker-stop)
		 ("C-c n t" . newsticker-start))
  :config
  (setq newsticker-download-logos    nil)
  :custom
  ;; Hacky hack, but it works... (see: custom-functions.el)
  (append-to-list 'newsticker-url-list
			   '(("NPR News"                "https://feeds.npr.org/1001/rss.xml")
				 ("WDWNT"                   "https://wdwnt.com/feed/")
				 ("Bleeping Computer"       "https://www.bleepingcomputer.com/feed/")
				 ("JElse's Blog"            "https://jlelse.blog/.atom")
				 ("KDE Blog"                "https://pointieststick.com/feed/")
				 ("Ars Technica"            "http://feeds.arstechnica.com/arstechnica/index/")
				 ("Kev Quirk's Blog"        "https://kevq.uk/feed/")
				 ("Oh Hello Ana"            "https://ohhelloana.blog/feed.xml")
				 ("Planet Emacs"            "https://planet.emacslife.com/atom.xml")
				 ("My Blog"                 "https://degruchy.org/feed.xml"))))
