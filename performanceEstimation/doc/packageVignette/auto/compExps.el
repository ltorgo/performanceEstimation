(TeX-add-style-hook "compExps"
 (lambda ()
    (LaTeX-add-bibliographies)
    (LaTeX-add-labels
     "sec:simpleEx"
     "fig:ex1Iris"
     "sec:expMeth"
     "fig:maeA1"
     "fig:maeA1b")
    (TeX-add-symbols
     "DR")
    (TeX-run-style-hooks
     "Sweave"
     "graphicx"
     "amssymb"
     "amsfonts"
     "amsmath"
     "inputenc"
     "utf8x"
     "latex2e"
     "art10"
     "article"
     "10pt"
     "a4paper")))

