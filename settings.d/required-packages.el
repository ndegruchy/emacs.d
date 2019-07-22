;; required-packages.el
;; These packages are required and will be loaded on launch of Emacs

(require 'package)

;; Package sources
;; List repositories to download files from

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable-mirror" . "https://www.mirrorservice.org/sites/stable.melpa.org/packages/"))

;; Fetch packages

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(eval-when-compile
  (require 'use-package))

;; Package list

(use-package avy
  :ensure t
  :bind (("M-g M-g" . avy-goto-line)
	 ("M-g c"   . avy-goto-char)
	 ("M-g w"   . avy-goto-word-0)))

(use-package bind-key
  :ensure t
  :after (use-package))

(use-package elfeed
  :ensure t
  :bind ("C-c f" . elfeed)
  :config
  (setq elfeed-feeds
	'("http://feeds.arstechnica.com/arstechnica/index"
	  "https://www.npr.org/rss/rss.php?id=1001"
	  "https://xkcd.com/atom.xml"
	  "https://planet.emacslife.com/atom.xml"
	  "https://www.archlinux.org/feeds/news/"
	  "https://www.linuxjournal.com/node/feed"
	  "https://www.phoronix.com/rss.php"
	  "https://www.gamingonlinux.com/article_rss.php")))

(use-package embrace
  :ensure t
  :after (expand-region)
  :bind (("C-c E" . embrace-commander)
	 ("C-c e" . embrace-add)))

(use-package emms
  :load-path "site-lisp/emms"
  :bind ("C-c M" . emms-smart-browse)
  :config
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)
  (emms-cache 1)
  (emms-cache-restore)
  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-libtag)
	emms-source-file-default-directory "/mnt/ndegruchy/Music"
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find))

(use-package expand-region
  :ensure t
  :bind ("C-c s" . er/expand-region))

(use-package ido-vertical-mode
  :ensure t)

(use-package no-littering
  :ensure t)

(use-package pdf-tools
  :load-path "site-lisp/pdf-tools"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package windresize
  :ensure t
  :bind ("C-c r" . windresize))
