(TeX-add-style-hook "compExps-knitr"
 (lambda ()
    (LaTeX-add-bibliographies
     "compExps")
    (LaTeX-add-labels
     "sec:simpleEx"
     "fig:ex1Iris"
     "sec:expMeth"
     "fig:maeA1"
     "fig:maeA1b")
    (TeX-add-symbols
     "PE")
    (TeX-run-style-hooks
     "breakurl"
     "url"
     "graphicx"
     "amssymb"
     "amsfonts"
     "amsmath"
     ""
     "inputenc"
     "utf8x"
     "latex2e"
     "art10"
     "article"
     "a4paper"
     "10pt")))

