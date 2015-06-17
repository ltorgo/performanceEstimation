# performanceEstimation

An R package providing an infra-structure for performance estimation and experimental comparison of predictive models.

**To Install the Latest Oficial Release (from CRAN) do the following in R:**

    install.packages("performanceEstimation")

**To Install the Latest Development Release (from github) do the following in R:**

    library(devtools)  # You need to install this package!
    install_github("ltorgo/performanceEstimation",ref="develop")

If this previous install_github call somehow fails (there are reports of problems with different libcurl library version on Linux hosts) you may try in alternative the following in R:

    library(devtools)
    install_git("https://github.com/ltorgo/peformanceEstimation",branch="develop")


After installation using any of the above procedures, the package can be used as any other R package by doing:

     library(performanceEstimation)

**A simple illustrative example:**

The following is a very simple example of how to use the package to obtain a 10-fold cross validation estimate of the error rate of an SVM on the famous **iris** data set:

    library(performanceEstimation)
    library(e1071)
    data(iris)
    r <- performanceEstimation(
                                PredTask(Species ~ ., iris),
                                Workflow(learner="svm"),
                                EstimationTask(metrics="err", method=CV())
			      )

After executing the previous code the user could obtain a general overview of the results either in the graphical or textual formats:

    plot(r)
    summary(r)

The above example is one of the simplest ways of using the package. For more information and examples of usage the accompanying package vignette, available as a PDF file through the standard R help system (after package installation), or available in the folder **report/packageVignette** of the current repository, contains an extensive description of the main functionalities of the package, as well as plenty of illustrative examples. On the same folder of this repository you will also find the R script file **performanceEstimation.R** that contains all R code of the examples in the package vignette, which will allow you to easily replicate all examples in that document.

Further help and illustrations can be obtained through the many help pages of each function defined in the package that contain lots of illustrative examples. Again, these help pages can be accessed as any other R package, through R help system (e.g. running **help.start()** at R command line)