\name{signifDiffs}
\alias{signifDiffs}

\title{
  Obtains a list with the set of paired differences are statistically
  significant

}
\description{
  This function receives as main argument the object resulting from a
  call to the \code{\link{pairedComparisons}} function and produces a
  list with the subset of the paired comparisons that are statistically
  significant given a certain \emph{p} value limit.
}
\usage{
signifDiffs(ps,p.limit=0.05,metrics=dimnames(ps)[[3]],tasks=dimnames(ps)[[4]])
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
  This function produces a list with as many components as there are
  tasks in the paired comparison. Each of the components is another list
  with as many components as there are metrics. Finally, each of these
  components in the inner sub-lists is a sub-set of the rows of the
  original object resulting from the paired comparison. This sub-set
  includes only the lines corresponding to workflows for which the
  paired comparison against the baseline workflow can be regarded as
  statistically significant, given the \code{p.limit} setting.
}
\value{
  The result of this function is a list.
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{pairedComparisons}},\code{\link{performanceEstimation}}
}
\examples{
\dontrun{
## Estimating MSE for 3 variants of both
## regression trees and SVMs, on  two data sets, using one repetition
## of 10-fold CV
library(e1071)
library(DMwR)
data(swiss)
data(mtcars)

## running the estimation experiment
res <- performanceEstimation(
  c(PredTask(Infant.Mortality ~ .,swiss),PredTask(mpg ~ ., mtcars)),
  c(workflowVariants("standardWF",learner="svm",
                     learner.pars=list(cost=c(1,10),gamma=c(0.01,0.5))),
    workflowVariants("standardWF",learner="rpartXse",
                     learner.pars=list(se=c(0,0.5,1)))
  ),
  CvSettings(nReps=1,nFolds=10,seed=1234)
  )

## carrying out parired comparisons against the workflow "svm.v2"
pcs <- pairedComparisons(res,"svm.v2")

## now let us check the significant differences with 95\% confidence
signifDiffs(pcs)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

