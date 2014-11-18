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
pairedComparisons(obj,baseline,
                  maxs=rep(FALSE,length(metricNames(obj))),
                  p.value=0.05)
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
  the workflow. If you omit this name the function will default to the
  name of the workflow resulting from a call to
  \code{\link{topPerformer}} on the first metric and first task.
}
  \item{maxs}{
    A vector of booleans with as many elements are there are metrics estimated in
    the experiment. A \code{TRUE} value means the respective
    metric is to be maximized, while a \code{FALSE} means
    minimization. Defaults to all \code{FALSE} values, i.e. all metrics are to
    be minimized.
  }
  \item{p.value}{
    A \emph{p} value to be used in the calculations that involve using
  values from statistical tables.
  }
}
\details{
  The \code{\link{performanceEstimation}} function allows you to obtain
  estimates of the expected value of a series of performance metrics for
  a set of alternative workflows and a set of predictive tasks. After
  running this type of experiments we frequently want to check if there
  is any statistical significance between the estimated performance of
  the different workflows. The current function allows you to carry out
  this type of checks. More specifically, the function provides: i) several
  statistical tests that check the statistical significance of the
  difference between the performance of a selected baseline workflow and
  the remaining alternatives; and ii) also tests for checking the
  statistical significance of all differences between all workflows. For
  the first type of checks the function calculates three tests: the
  paired t.test, the paired Wilcoxon Signed Rank test and the method
  recommended by Demsar (2006)  consisting of a Friedman test followed
  by a post-hoc Bonferroni-Dunn test. For the second type of checks we
  also follow the recommendations of Demsar (2006) and provide a
  Friedman test followed by the post-hoc Nemenyi test.

  The \code{\link{performanceEstimation}} function ensures that all
  compared workflows are run on exactly the same train+test partitions
  on all repetitions and for all predictive tasks. In this context, we
  can use pairwise statistical significance tests.

  Although we provide different statistical tests, we recommend you
  follow the proposals outlined in Demsar (2006) regards this type of
  hypothesis testing. Namely, if your goal is to compare a set of
  alternative workflows against a baseline workflow we recommend you use
  the information from the Friedman test followed by the post-hoc
  Bonferroni-Dunn test. In case your goal is to find out any
  statistically significant differences among all workflows, then we
  recommend the same Friedman test followed by the Nemenyi post-hoc
  test. For both of these two paths we provide an implementation of the
  diagrams (CD diagrams) described in Demsar (2006) through the
  functions \code{\link{CDdiagram.BD}} and \code{\link{CDdiagram.Nemenyi}}.
}
\value{
  The result of this function is the information from performing all
  these statistical tests. This information is returned as a list with
  as many components as there are estimated metrics. For each metric a
  list with several components describing the results of these tests is
  provided.

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

## obtaining a CD diagram comparing all others against svm.v2 in terms
## of error rate
CDdiagram.BD(pres,metric="err")

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

