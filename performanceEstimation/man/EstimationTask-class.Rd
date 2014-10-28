\name{EstimationTask-class}
\Rdversion{1.1}
\docType{class}
\alias{EstimationTask-class}
\alias{show,EstimationTask-method}

\title{Class \code{"EstimationTask"}}
\description{
  This class of objects contains the information describing an
  estimation task.
}
\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("EstimationTask",...)}
providing the values for the class slots. These objects contain
information on the metrics to be estimated, as well as on the estimation
method to use to obtain the estimates. Moreover, in case you want to use
metrics not currently implemented by this package you can also provide
the name of a function (and its parameters) that will be called to
calculate these metrics.
}
\section{Slots}{
  \describe{
    \item{\code{metrics}:}{A vector of objects of class
      \code{character} containing the names of the metrics to be
      estimated. These can be any of the metrics provided by the
      functions \code{\link{classificationMetrics}} and
      \code{\link{regressionMetrics}} or "trTime", "tsTime" or "totTime" for
      training, testing and total time, respectively. You may also
      provide the name of any other metrics, but in that case you need
      to use the slots \code{evaluator} and \code{evaluator.pars} to
      indicate the function to be use to calculate them.}
    \item{\code{evaluator}:}{An optional object of class
      \code{character} containing the name of a function to be used to
    calculate the specified metrics. It will default to either
    \code{\link{classificationMetrics}} or
    \code{\link{regressionMetrics}} depending on the type of prediction
    task.}
    \item{\code{evaluator.pars}:}{An optional  list containing
      the parameters to be passed to the function calculating the metrics.}
    \item{\code{method}:}{Object of class \code{EstimationSettings}
      containing the estimation method and its settings to be used to
      obtain the estimates of the metrics (defaulting to \code{CV()}). }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "EstimationTask")}: method used to
      show the contents of a \code{EstimationTask} object. }
	 }
}
\references{
Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}
}
\author{
Luis Torgo \email{ltorgo@dcc.fc.up.pt} 
}
\seealso{
   \code{\linkS4class{MonteCarlo}},
   \code{\linkS4class{CV}},
   \code{\linkS4class{LOOCV}},
   \code{\linkS4class{Bootstrap}},
   \code{\linkS4class{Holdout}},
   \code{\linkS4class{EstimationMethod}}
}
\examples{
showClass("EstimationTask")

## Estimating Mean Squared Error using 10-fold cross validation
et <- EstimationTask(metrics="mse")
et

## Estimating Accuracy and Total Time (train+test times) using 3
## repetitions of holdout with 20\% of the cases used for testing. 
EstimationTask(metrics=c("acc","totTime"),method=Holdout(nReps=3,hldSz=0.2))

## An example with a user-defined metric: the average differences between true
## predicted values raised to a certain power.

## The first we define the function that calculates this metric. It
## needs to have 'trues' and 'preds' as the first two arguments
powErr <- function(trues,preds,pow=3,...) {
     c(pow.err = mean((trues-preds)^pow))
}

## Now the estimation task (10-fold cv in this example)
EstimationTask(metrics="pow.err",
               evaluator="powErr",
               evaluator.pars=list(pow=4),
               method=CV())
}
\keyword{classes}
