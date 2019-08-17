;; Custom Skeletons for starting up new files easily.
;; OOoooOoo Spooky!

(define-skeleton ndegruchy/letter
  "Inserts a Latex letter skeleton into current buffer.
This only makes sense for empty buffers."
  "Title: "
  "\\documentclass{letter}\n"
  "\\usepackage[latin1]{inputenc}\n"
  "\\name{Nathan DeGruchy}\n"
  "\\begin{document}\n"
  "\\begin{letter}{" str | " *** Title *** " "}\n"
  "\\opening{" _ "}\n\n"
  "\\end{letter}\n"
  "\\end{document}\n")
