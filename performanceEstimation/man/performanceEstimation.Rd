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
performanceEstimation(tasks,workflows,setts,...)
}

\arguments{
  \item{tasks}{
This is a list of objects of class \code{\linkS4class{PredTask}}, containing the
  predictive tasks that will be used in the estimation procedure.
}
  \item{workflows}{
This is a list of objects of class \code{\linkS4class{Workflow}}, containing the
  workflows representing different approaches to the predictive tasks,
  and whose performance we want to estimate.

}
  \item{setts}{
This is an object belonging to any of the sub-classes of the virtual
  class \code{\linkS4class{EstimationSettings}}. It is the class of this object that
  determines the type of estimation methodology that will be used.  See
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

  Currently, the function allows for 5 different types of estimation
  methods to be used. These are different  methods for providing
  reliable estimates of the true value of the selected evaluation  statistics. The method to be used is determined by the class of the object
  provided in the argument \code{setts}. The following are the
  possibilities:

  \emph{Cross validation}: this type of estimates can be obtained by
  providing in the \code{setts} argument an object of class
  \code{\linkS4class{CvSettings}}. More details on this type of experiments can be
  obtained in the help page of the function \code{\link{cvEstimates}}.

  \emph{Leave one out cross validation}: this type of estimates can be obtained by
  providing in the \code{setts} argument and object of class
  \code{\linkS4class{LoocvSettings}}. More details on this type of experiments can be
  obtained in the help page of the function \code{\link{loocvEstimates}}.

   \emph{Hold out}: this type of estimates can be obtained by
  providing in the \code{setts} argument and object of class
  \code{\linkS4class{HldSettings}}. More details on this type of experiments can be
  obtained in the help page of the function \code{\link{hldEstimates}}.
  
   \emph{Monte Carlo}: this type of estimates can be obtained by
  providing in the \code{setts} argument and object of class
  \code{\linkS4class{McSettings}}. More details on this type of experiments can be
  obtained in the help page of the function \code{\link{mcEstimates}}.

   \emph{Bootstrap}: this type of estimates can be obtained by
  providing in the \code{setts} argument and object of class
  \code{\linkS4class{BootSettings}}. More details on this type of experiments can be
  obtained in the help page of the function \code{\link{bootEstimates}}.

  Further functions within this package can then be used to analyse the
  results of the estimation process.

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
  c(workflowVariants("standardWF",learner="svm",
                     learner.pars=list(cost=c(1,10),gamma=c(0.01,0.5))),
    workflowVariants("standardWF",learner="rpartXse",
                     learner.pars=list(se=c(0,0.5,1)))
  ),
  CvSettings(nReps=1,nFolds=10,seed=1234)
  )

## Check a summary of the results
summary(res)

## best performers for each metric and task
topPerformers(res)

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models}

