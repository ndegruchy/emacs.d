;;; Web Skeletons! Boooo!
;;       .-.
;;      (o.o)
;;       |=|
;;      __|__
;;    //.=|=.\\
;;   // .=|=. \\
;;   \\ .=|=. //
;;    \\(_=_)//
;;     (:| |:)
;;      || ||
;;      () ()
;;      || ||
;;      || ||
;; l42 ==' '==

(define-skeleton ndegruchy/skeleton-web-article
	"Insert a degruchy-org article HTML skeleton"
	nil
	'(setq title  (skeleton-read "Article Title: "))
	'(setq dashed (ndegruchy/dasherize title))
	'(setq date   (format-time-string "%Y%m%d"))
	'(setq anchor (concat date "-" dashed))
	> "<article class=\"hentry h-entry\" id=\"" anchor "\">\n"
	> "<header>\n"
	> "<h2 class=\"p-name entry-title\">\n"
	> title "\n"
	> "<a href=\"#" anchor "\" class=\"u-url\">&#x1F517;</a>\n"
	> "</h2>\n"
	> "<time class=\"dt-published published\" datetime=\"" (format-time-string "%FT%T%z") "\">" (format-time-string "%T") " on " (format-time-string "%Y-%m-%d") "</time>\n"
	> "</header>\n"
	> "\t<section class=\"article-content e-content\">" _ "</section>\n"
	> "</article>\n")

(define-skeleton ndegruchy/skeleton-web-gallery
	"Skeleton for creating image galleries"
	nil
	'(setq date (format-time-string "%Y-%m-%d"))
	> "<section class=\"gallery\">\n"
	> "<ul>\n"
	> "<li>\n"
	> "<a href=\"/assets/images/posts/" date "/\">\n"
	> "<figure>\n"
	> "<picture>\n"
	> "<source srcset=\"/assets/images/posts/" date "/\" type=\"image/heic\">\n"
	> "<source srcset=\"/assets/images/posts/" date "/\" type=\"image/webp\">\n"
	> "<img href=\"/assets/images/posts/" date "/\" alt=\"\" height=\"128\" width=\"128\" loading=\"lazy\">\n"
	> "</picture>\n"
	> "<figcaption>" _ "</figcaption>\n"
	> "</figure>\n"
	> "</a>\n"
	> "</li>\n"
	> "</ul>\n")

(define-skeleton ndegruchy/skeleton-web-picture
	"Inserts a skeleton for a picture on a website"
	nil
	'(setq date (format-time-string "%Y-%m-%d"))
	> "<figure>\n"
	> "<picture>\n"
	> "<source srcset=\"/assets/images/posts/" date "/\" type=\"image/heic\">\n"
	> "<source srcset=\"/assets/images/posts/" date "/\" type=\"image/webp\">\n"
	> "<img href=\"/assets/images/posts/" date "/\" alt=\"\" height=\"\" width=\"\" loading=\"lazy\">\n"
	> "</picture>\n"
	> "<figcaption>" _ "</figcaption>\n"
	> "</figure>\n")

(define-skeleton ndegruchy/skeleton-web-atom-entry
	"ATOM entry skeleton for updating feed.xml"
	nil
	'(setq title  (skeleton-read "Article Title: "))
	'(setq dashed (ndegruchy/dasherize title))
	'(setq date   (format-time-string "%Y%m%d"))
	'(setq anchor (concat date "-" dashed))
	> "<entry>\n"
	> "<title>" title "</title>\n"
	> "<id>https://degruchy.org/#" anchor "</id>\n"
	> "<link rel=\"alternate\" href=\"https://degruchy.org/#" anchor "\" />\n"
	> "<author>\n"
	> "<name>Nathan DeGruchy</name>\n"
	> "<email>nathan@degruchy.org</email>\n"
	> "</author>\n"
	> "<updated>" (format-time-string "%FT%T%z") "</updated>\n"
	> "<published>" (format-time-string "%FT%T%z") "</published>\n"
	> "<content />\n"
	> "</entry>\n")
