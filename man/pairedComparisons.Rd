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
  of their performances across these problems. This function implements
  several statistical tests that can be used to test several hypothesis
  concerning the observed differences in performance between the
  workflows on the tasks. 
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
    Several tests involve the hypothesis that a certain workflow is
  significantly different from a set of other alternatives. This
  argument allows you to specify the name of this baseline workflow. If
  you omit this name the function will default to the 
  name of the workflow that has the lower average rank position across
  all tasks, for each estimation metric.
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
  values from statistical tables (defaults to 0.05).
  }
}
\details{
  The \code{\link{performanceEstimation}} function allows you to obtain
  estimates of the expected value of a series of performance metrics for
  a set of alternative workflows and a set of predictive tasks. After
  running this type of experiments we frequently want to check if there
  is any statistical significance between the estimated performance of
  the different workflows. The current function allows you to carry out
  this type of checks.

  The function will only run on experiments containing more than one
  workflow as paired comparisons do not make sense with one single
  alternative workflow. Having more than one workflow we can distinguish
  two situations: i) comparing the performance of two workflows; or
  ii) comparisons among multiple workflows. The recommendations for checking  the statistical
  significance of the  difference between the performance of the
  alternative workflows varies within these two setups (see Demsar
  (2006) for recommendations).

  The current function implements several statistical tests that can be
  used for different hypothesis tests. Namely, it obtains 
  the results of paired \emph{t} tests and paired \emph{Wilcoxon Signed
  Rank} tests for situations where you are comparing the performance of
  two workflows, with the latter being recommended given the typical
  overlap among the training sets that does not ensure independence
  among the scores of the different iterations. For the setup of
  multiple workflows on multiple tasks the function also calculates the \emph{Friedman} test
  and the post-hoc \emph{Nemenyi} and \emph{Bonferroni-Dunn} tests,
  according to the procedures described in Demsar (2006). The
  combination \emph{Friedman} test followed by the post-hoc
  \emph{Nemenyi} test is recommended when you want to carry out paired
  comparisons between all alternative workflows on the set of tasks to
  check for which differences are significant. The combination
  \emph{Friedman} test followed by the post-hoc \emph{Bonferroni-Dunn}
  test is recommended when you want to compare a set of alternative
  workflows against a baseline workflow. For both of these two paths we
  provide an implementation of the  diagrams (CD diagrams) described in
  Demsar (2006) through the  functions \code{\link{CDdiagram.BD}} and
  \code{\link{CDdiagram.Nemenyi}}. 

  The \code{\link{performanceEstimation}} function ensures that all
  compared workflows are run on exactly the same train+test partitions
  on all repetitions and for all predictive tasks. In this context, we
  can use pairwise statistical significance tests.

}
\value{
  The result of this function is the information from performing all
  these statistical tests. This information is returned as a list with
  as many components as there are estimated metrics. For each metric a
  list with several components describing the results of these tests is
  provided.
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

