(TeX-add-style-hook "performanceEstimation"
 (lambda ()
    (LaTeX-add-bibliographies)
    (LaTeX-add-labels
     "sec:simpleEx"
     "sec:expMeth")
    (TeX-add-symbols
     "PE")
    (TeX-run-style-hooks
     "breakurl"
     "fancyvrb"
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

