(TeX-add-style-hook "performanceEstimation"
 (lambda ()
    (LaTeX-add-bibliographies)
    (LaTeX-add-labels
     "sec:install"
     "sec:simpleEx"
     "sec:variants"
     "sec:standardWF"
     "sec:timeseriesWF"
     "sec:expMeth"
     "sec:MC"
     "fig:MC")
    (TeX-add-symbols
     "PE"
     "PEversion")
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

