;; Custom Skeletons file, use in place of yasnippet

(define-skeleton ndegruchy/doart
  "Insert a degruchy-org article HTML skeleton"
  nil
  "<article class=\"hentry h-entry\" id=\"date-anchor\">\n"
  "\t<header>\n"
  "\t\t<h2 class=\"p-name entry-title\">\n"
  "\t\t\tTitle\n"
  "\t\t\t<a href=\"#date-anchor\" class=\"u-url\">🔗</a>\n"
  "\t\t</h2>\n"
  "\t\t<time class=\"dt-published published\" datetime=\"ISODATE\">time on date</time>\n"
  "\t</header>\n"
  "\t<section class=\"article-content e-content\">$0</section>\n"
  "\t<footer>\n"
  "\t\t<address class=\"p-author author hcard h-card\">\n"
  "\t\t\t<img src=\"/assets/images/2020-minimal-avatar-50.png\" alt=\"Me!\" class=\"u-photo\" height=\"50\" width=\"50\" loading=\"lazy\">\n"
  "\t\t\t<a href=\"/\" class=\"u-url\">Nathan DeGruchy</a>\n"
  "\t\t</address>\n"
  "\t</footer>\n"
  "</article>\n")
