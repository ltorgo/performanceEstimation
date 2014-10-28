\name{pairedComparisons}
\alias{pairedComparisons}

\title{
  Statistical hypothesis testing on the observed paired differences in
  estimated performance. 
}
\description{
  This function analyses the statistical significance of the paired
  comparisons between the estimated performance scores of a set of
  workflows.  When you run the \code{performanceEstimation()} function to
  compare a set of workflows over a set of problems you obtain estimates
  of their performances across these problems. This function allows you
  to test the hypothesis that the difference between the estimated score
  of a baseline workflow and the remaining alternatives is statistically
  significant. 
}
\usage{
pairedComparisons(obj,baseline,test="wilcoxon")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
    An object of class \code{\linkS4class{ComparisonResults}} 
    that contains the results of a performance estimation experiment. 
  }
  \item{baseline}{
    When you carry out this type of analysis you have to select the
  workflow against which all paired comparisons will be carried
  out. This parameter should contain a character string with the name of
  the workflow
}
\item{test}{
  A character string with the name of the statistical test to use to
  check the hypothesis that there is no singificant difference between
  the observed averages (defaults to "wilcoxon"). See Details section.
}
}
\details{
  The \code{\link{performanceEstimation}} function allows you to obtain
  estimates of the expected value of a series of performance metrics for
  a set of alternative workflows and a set of predictive tasks. After
  running this type of estimation experiments it is natural that one
  tries to check what is the best performing workflow for each
  task/metric. This can be obtained with the function
  \code{\link{topPerformers}}. From this analysis it is possible that we
  are able to identify one particular workflow as the "best" solution
  to these tasks. In this context, we frequently want to check to what
  extent we can be confident that the observed differences between this
  selected workflow and the remaining alternatives are statistically
  significant. This is the goal of the current function. It allows you
  to carry out paired comparisons between a selected workflow and a set
  of alternatives. The goal of these comparisons is to check to what
  extent we can reject the null hypothesis that the difference between
  the estimated average performances is zero. The result is thus a set
  of \emph{p} values for each paired comparison, which indicate the
  confidence level on the rejection of the null hypothesis.

  The \code{\link{performanceEstimation}} function ensures that all compared workflows
are run on exactly the same train+test partitions on all repetitions and
for all predictive tasks. In this context, we can use pairwise
statistical significance tests. Given that, with the exception of a
single repetition of hold-out,  we cannot ensure that the
different iterations are statistically independent (for instance there
may be some overlap between the training samples), we recommend the use
of the Wilcoxon signed rank test to assess the statistical significance of the
differences between every pair of compared workflows. This is the
default for the \code{test} parameter. However, this function also
implements paired t-tests which could be preferred on single repetitions
of hold-out estimation experiments.
}
\value{
  The result of this function is an array with four dimensions. The
  first dimension contains the workflows (the first of these being the
  selected baseline), the second dimension contains the results of the
  test. These results are: i) the average estimate score, ii) the standard
  error of this estimate, iii)
  the difference between the average score of the baseline and
  the average score of the workflow in the row, and  iv) the \emph{p}
  value of the corresponding paired comparison that tests the validity
  of the null hypothesis that the difference is zero. The third
  dimension of the array are the evaluation metrics, while the fourth
  dimension contains the predictive tasks.
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\link{signifDiffs}},
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
  CV(nReps=1,nFolds=10,seed=1234)
  )

## checking the top performers
topPerformers(res)

## now let us assume that we will choose "svm.v2" as our baseline
## carry out the paired comparisons
pairedComparisons(res,"svm.v2")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

