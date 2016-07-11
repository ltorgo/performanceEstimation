\name{EstimationTask-class}
\Rdversion{1.1}
\docType{class}
\alias{EstimationTask}
\alias{EstimationTask-class}
\alias{show,EstimationTask-method}

\title{Class \code{"EstimationTask"}}
\description{
  This class of objects contains the information describing an
  estimation task.
}
\section{Objects from the Class}{
Objects can be created by calls of the form
\code{EstimationTask(...)}
providing the values for the class slots. These objects contain
information on the metrics to be estimated, as well as on the estimation
method to use to obtain the estimates. Moreover, in case you want to use
metrics not currently implemented by this package you can also provide
the name of a function (and its parameters) that will be called to
calculate these metrics.
}
\section{Slots}{
  \describe{
    \item{\code{metrics}:}{An optional vector of objects of class
      \code{character} containing the names of the metrics to be
      estimated. These can be any of the metrics provided by the
      functions \code{\link{classificationMetrics}} and
      \code{\link{regressionMetrics}} or "trTime", "tsTime" or "totTime" for
      training, testing and total time, respectively. You may also
      provide the name of any other metrics, but in that case you need
      to use the slots \code{evaluator} and \code{evaluator.pars} to
      indicate the function to be use to calculate them. If you do not supply 
      the name of any metric, all metrics of the used evaluator function will
      be calculated.}
    \item{\code{method}:}{Object of class \code{\linkS4class{EstimationMethod}}
      containing the estimation method and its settings to be used to
      obtain the estimates of the metrics (defaulting to \code{CV()}). }
    \item{\code{evaluator}:}{An optional object of class
      \code{character} containing the name of a function to be used to
      calculate the specified metrics. It will default to either
      \code{\link{classificationMetrics}} or
      \code{\link{regressionMetrics}} depending on the type of prediction
      task.}
    \item{\code{evaluator.pars}:}{An optional  list containing
      the parameters to be passed to the function calculating the metrics.}
    \item{\code{trainReq}:}{An optional  logical value indicating
      whether the metrics to be calculated require that the training
      data is supplied (defaulting to \code{FALSE}). Note that if the
      user selects any of the metrics "nmse", "nmae" or "theil" this
      will be set to \code{TRUE}.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "EstimationTask")}: method used to
      show the contents of a \code{EstimationTask} object. }
  }
}
\details{
 In case you are providing your own user-defined evaluator functions
 (through parameters \code{evaluator} anbd \code{evaluator.pars}) you
 need to follow some protocol in defining these functions so that the
 package may correctly call them during the execution of the estimation
 experiments. This protocol depends on the output of the workflows you 
 plan to evaluate with your use-defined function. Standard workflows 
 (\code{\link{standardWF}} or \code{\link{timeseriesWF}}) will return 
 at least a vector named \code{trues} with  the true values of the test 
 cases and the vector named \code{preds} with the respective predicted 
 values (in this order). This means that your evaluator function should 
 assume that it will be called with these two vectors as the first two 
 arguments. However, if you are not using the standard workflows, you 
 have more flexibility. In effect, user-defined workflows return whatever 
 the author wants them to return (unless they are going to be evaluated 
 using either \code{\link{classificationMetrics}} or 
 \code{\link{regressionMetrics}}, that require the vectors of true and 
 predicted values). This means that in the most flexible case where you 
 have your own user-defined workflow function and your user-defined evaluator 
 function, you can use whatever parameters you want. The only thing you need 
 to worry is to be aware that your user-defined evaluator function will 
 be called with whatever your user-defined workflow has returned as result 
 of its execution. Your user-defined evaluator function should calculate 
 whatever metrics are indicated through the parameter \code{stats} that 
 is a vector of strings. In case the slot \code{trainReq} is \code{TRUE} 
 then the user-defined evaluator function should also have a parameter
 named \code{train.y} that will "receive" the values of the
 target variable on the training set. The remaining parameters of the
 user-defined function can be freely defined by you and their values
 will be specified through the contents of the \code{evaluator.pars}
 list.
 
}
\references{
Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}
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

## First we define the function that calculates this metric. It
## needs to have 'trues' and 'preds' as the first two arguments if we
## want it to be usable by standard workflows
powErr <- function(trues,preds,metrics,pow=3) {
     if (metrics != "pow.err") stop("Unable to calculate that metric!")
     c(pow.err = mean((trues-preds)^pow))
}

## Now the estimation task (10-fold cv in this example)
EstimationTask(metrics="pow.err", method=CV(), 
               evaluator="powErr", evaluator.pars=list(pow=4))
}
\keyword{classes}
