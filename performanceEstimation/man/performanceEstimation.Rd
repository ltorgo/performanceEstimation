\name{performanceEstimation}
\alias{performanceEstimation}

\title{
  Estimate the predictive performance of modeling alternatives on
  different predictive tasks
}
\description{
This function can be used to estimate the predictive performance of
  alternative approaches to a set of predictive tasks, using different
  estimation methods. This is
a generic function that should work with any modeling approaches provided a
few assumptions are met. The function implements different estimation procedures, namely: cross validation, leave one out cross validation,
hold-out, monte carlo simulations and bootstrap.
}
\usage{
performanceEstimation(tasks,workflows,estTask,...)
}

\arguments{
  \item{tasks}{
This is a vector of objects of class \code{\linkS4class{PredTask}}, containing the
  predictive tasks that will be used in the estimation procedure.
}
  \item{workflows}{
This is a vector of objects of class \code{\linkS4class{Workflow}}, containing the
  workflows representing different approaches to the predictive tasks,
  and whose performance we want to estimate.

}
  \item{estTask}{
This is an object belonging to class
\code{\linkS4class{EstimationTask}}. It is used to specify the metrics
  to be estimated and the method to use to obtain these estimates.  See
  section Details for the possible values. 
}
  \item{...}{
Any further parameters that are to be passed to the lower-level
  functions implementing each individual estimation methodology. 
}
}
\details{
The goal of this function is to allow estimating the perfomance of a set
of alternative modelling approaches to a set of predictive tasks. The estimation
  can be carried out using different methodologies. All alternative
  approaches (which we will refer to as \emph{workflows}) will be
  applied using the same exact data partitions for each  task thus
  ensuring the possibility of carrying out paired  comparisons using
  adequate statistical tests for checking the significance of the
  observed differences in performance.

  The first parameter of this function is a vector of
  \code{\linkS4class{PredTask}} objects that define the tasks to use in
  the estimation process.

  The second argument is a vector of \code{\linkS4class{Workflow}}
  objects. These can be created in two different ways: either directly
  by calling the constructor of this class; or by using the
  \code{\link{workflowVariants}} function that can be used to
  automatically generate different workflow objects as variants of some
  base workflow. Either way there are two types of workflows:
  user-defined workflows and what we call "standard" workflows. The
  later are workflows that people typically follow to solve predictive
  tasks and that are already implemented in this package to facilitate
  the task of the user. These standard workflows are implemented in
  functions \code{\link{standardWF}} and
  \code{\link{timeseriesWF}}. When specifying the vector of workflows if
  you use (either in the constructor or in the function
  \code{\link{workflowVariants}}) the parameter \code{user}, the package
  will assume that you will be providing your own workflow function. If
  instead you supply a value for parameter \code{learner} the package
  assumes you are using a "standard" workflow. If on top of supplying a
  value for \code{learner} you also supply a value for \code{type}
  (either 'slide' or 'grow'), the \code{\link{timeseriesWF}} will be
  used, otherwise the function uses \code{\link{standardWF}}. See the
  Examples section and the accompanying vignette for illustrations of
  all these combinations. Summarizing, in terms of workflows you can
  use: i) your own user-defined workflows; ii) the standard workflow
  implemented by function \code{\link{standardWF}}; or iii) the standard
  time series workflow implementd by \code{\link{timeseriesWF}}.

  Currently, the function allows for 5 different types of estimation
  methods to be used. These are different  methods for providing
  reliable estimates of the true value of the selected evaluation
  metrics. Both the metrics and the estimation method are defined
  through the value provided in argument \code{estTask}. The 5
  estimation methodologies are the following:

  \emph{Cross validation}: this type of estimates can be obtained by
  providing in \code{estTask} argument an object of class
  \code{\linkS4class{EstimationTask}} with \code{method} set to an
  object of class \code{\linkS4class{CV}} (this is the default). More
  details on this type of method can be 
  obtained in the help page of the class \code{\linkS4class{CV}}.

  \emph{Leave one out cross validation}: this type of estimates can be obtained by
  providing in \code{estTask} argument an object of class
  \code{\linkS4class{EstimationTask}} with \code{method} set to an
  object of class \code{\linkS4class{LOOCV}}. More
  details on this type of method can be 
  obtained in the help page of the class \code{\linkS4class{LOOCV}}.

  \emph{Hold out}: this type of estimates can be obtained by
  providing in \code{estTask} argument an object of class
  \code{\linkS4class{EstimationTask}} with \code{method} set to an
  object of class \code{\linkS4class{Holdout}}. More
  details on this type of method can be 
  obtained in the help page of the class \code{\linkS4class{Holdout}}.
  
  \emph{Monte Carlo}: this type of estimates can be obtained by
  providing in \code{estTask} argument an object of class
  \code{\linkS4class{EstimationTask}} with \code{method} set to an
  object of class \code{\linkS4class{MonteCarlo}}. More
  details on this type of method can be 
  obtained in the help page of the class \code{\linkS4class{MonteCarlo}}.

  \emph{Bootstrap}: this type of estimates can be obtained by
  providing in \code{estTask} argument an object of class
  \code{\linkS4class{EstimationTask}} with \code{method} set to an
  object of class \code{\linkS4class{Bootstrap}}. More
  details on this type of method can be 
  obtained in the help page of the class \code{\linkS4class{Bootstrap}}.

}
\value{
  The result of the function is an object of class \code{\linkS4class{ComparisonResults}}.
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\link{workflowVariants}},\code{\link{topPerformers}},\code{\link{rankWorkflows}},
  \code{\link{pairedComparisons}}, \code{\link{cvEstimates}}, \code{\link{loocvEstimates}},
\code{\link{hldEstimates}}, \code{\link{mcEstimates}},
  \code{\link{bootEstimates}}

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
  c(workflowVariants(learner="svm",
                     learner.pars=list(cost=c(1,10),gamma=c(0.01,0.5))),
    workflowVariants(learner="rpartXse",
                     learner.pars=list(se=c(0,0.5,1)))
  ),
  EstimationTask(metrics="mse")
  )

## Check a summary of the results
summary(res)

## best performers for each metric and task
topPerformers(res)


## Estimating the accuracy of a default SVM on IRIS using 10 repetitions
## of a 80\%-20\% Holdout
data(iris)
res1 <- performanceEstimation(PredTask(Species  ~ .,iris),
             Workflow(learner="svm"),
             EstimationTask(metrics="acc",method=Holdout(nReps=10,hldSz=0.2)))
summary(res1)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models}

