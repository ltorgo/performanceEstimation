\name{signifDiffs}
\alias{signifDiffs}

\title{
  Obtains a list with the set of paired differences that are statistically
  significant according to a p-value threshold

}
\description{
  This function receives as main argument the object resulting from a
  call to the \code{\link{pairedComparisons}} function and produces a
  list with the subset of the paired comparisons using the \emph{t} test and
  the \emph{Wilcoxon Signed Rank} test that are statistically
  significant given a certain \emph{p} value limit.
}
\usage{
signifDiffs(ps, p.limit=0.05,
            metrics=names(ps),
            tasks=rownames(ps[[1]]$avgScores))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ps}{
    An object resulting from a call to the
    \code{\link{pairedComparisons}} function. 
  }
  \item{p.limit}{
    A number indicating the maximum value of the confidence level
    (p.value) of the statistical hypothesis test for a paired comparison
    to be considered statistically significant (defaults to 0.05). All
    paired comparisons with a \emph{p} value below this limit will
    appear in the results of this function.
  }
  \item{metrics}{
    A vector with the names of the metrics for which we want the results
    (defaults to all metrics included in the paired comparison).
  }
  \item{tasks}{
    A vector with the names of the prediction tasks for which we want the results
    (defaults to all tasks included in the paired comparison).
  }
}
\details{
  This function produces a list with as many components as the selected
  metrics (defaulting to all metrics in the paired comparison). Each of
  the components is another list with two components: i) one with the
  results for the \emph{t} tests; and ii) the other with the results for
  the \emph{Wilcoxon Signed Rank} test. Each of these two components is
  an array with 3 dimensions, with the rows representing the workflows,
  the columns a set of statistics and the thrid dimension being the
  task. The first row of these arrays will contain the baseline workflow
  against which all others are being compared (by either the \emph{t}
  test or through the \emph{Wilcoxon Signed Rank} test). The remaining
  rows will include the workflows whose comparison against this baseline
  is statistically significant, i.e. whose \emph{p} value of the paired
  comparison is below the provided \emph{p} limit.
}
\value{
  The result of this function is a list (see the Details section).
}
\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\link{pairedComparisons}},
  \code{\link{performanceEstimation}}
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

## now let us assume that we will choose "svm.v2" as our baseline
## carry out the paired comparisons
pres <- pairedComparisons(res,"svm.v2")

## Obtaining the subset of differences that are significant
## with 99\% confidence 
sds <- signifDiffs(res,p.limit=0.01)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

