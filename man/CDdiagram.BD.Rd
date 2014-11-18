\name{CDdiagram.BD}
\alias{CDdiagram.BD}

\title{
CD diagrams for the post-hoc Boferroni-Dunn test
}
\description{
This function obtains a Critical Difference (CD) diagram for the
post-hoc Bonferroni-Dunn test along the lines defined by Demsar (2006). These
diagrams provide an interesting visualization of the statistical
significance of the observed paired differences between a set of
workflows and a selected baseline wrokflow. They allow us to compare a
set of alternative workflows against this baseline and answer the
question whether the differences are statisticall y significant.
}
\usage{
CDdiagram.BD(r, metric = names(r)[1])
}

\arguments{
  \item{r}{
    A list resulting from a call to \code{\link{pairedComparisons}}
  }
  \item{metric}{
    The metric for which the CD diagram will be obtained (defaults to the
    first metric of the comparison).
  }
}
\details{
  Critical Difference (CD) diagrams are interesting sucint
  visualizations of the results of a Bonferroni-Dunn post-hoc test that is
  designed to check the statistical significance between the differences
  in average rank of a set of workflows against a baseline workflow, on
  a set of predictive tasks.
  
  In the resulting graph each workflow is represented by a colored
  line. The X axis where the lines end represents the average rank position
  of the respective workflow across all tasks. The null hypothesis is that
  the average rank of a baseline workflow does not differ with
  statistical significance (at some confidence level defined in the call
  to \code{\link{pairedComparisons}} that creates the object used to
  obtain these graphs) from the average ranks of a set of
  alternative workflows. An horizontal line connects the baseline
  workflow with the alternative workflows for which we cannot reject
  this hypothesis. This means that only the alternative workflows that
  are not connect with the baseline can be considered as having an
  average rank that is different from the one of the baseline with
  statistical significance. 
}
\value{
Nothing, the graph is draw on the current device.
}
\references{
  Demsar, J. (2006) \emph{Statistical Comparisons of Classifiers over
    Multiple Data Sets}. Journal of Machine Learning Research, 7, 1-30.
  
  Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\link{CDdiagram.Nemenyi}},
  \code{\link{CDdiagram.BD}},  
  \code{\link{signifDiffs}},
  \code{\link{metricNames}},
  \code{\link{performanceEstimation}},
  \code{\link{topPerformers}},
  \code{\link{topPerformer}},
  \code{\link{rankWorkflows}},
  \code{\link{metricsSummary}},
  \code{\linkS4class{ComparisonResults}}
}
\examples{
\dontrun{
## Estimating MSE for 3 variants of both
## regression trees and SVMs, on  two data sets, using one repetition
## of 10-fold CV
library(e1071)
data(iris)
data(Satellite,package="mlbench")
data(LetterRecognition,package="mlbench")


## running the estimation experiment
res <- performanceEstimation(
           c(PredTask(Species ~ .,iris),
             PredTask(classes ~ .,Satellite,"sat"),
             PredTask(lettr ~ .,LetterRecognition,"letter")),
           workflowVariants(learner="svm",
                 learner.pars=list(cost=1:4,gamma=c(0.1,0.01))),
           EstimationTask(metrics=c("err","acc"),method=CV()))


## checking the top performers
topPerformers(res)

## now let us assume that we will choose "svm.v2" as our baseline
## carry out the paired comparisons
pres <- pairedComparisons(res,"svm.v2")

## obtaining a CD diagram comparing all workflows against
## the baseline (defined in the previous call to pairedComparisons)
CDdiagram.BD(pres,metric="err")

}
}

\keyword{ models }

