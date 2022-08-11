;; LaTeX (TeX) Skeletons. Ball-gag not included.
(define-skeleton ndegruchy/latex-base
  "Default LaTeX file contents"
  "Title: "
  "\\documentclass[11pt]{article}\n"
  "\\title{" str | " ENTER TITLE HERE " "}\n"
  "\\begin{document}\n"
  "\\maketitle\n"
  "\n" _ "\n\n"
  "\\end{document}")
