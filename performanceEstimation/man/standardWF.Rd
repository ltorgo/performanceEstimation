\name{standardWF}
\alias{standardWF}

\title{
  A function implementing a standard workflow for prediction tasks
}
\description{
  This function implements a standard workflow for both  classification
  and regression tasks. The workflow consists of: (i) learning a
  predictive model based on the given training set, (ii) using it to
  make predictions for the provided test set,  and finally  (iii)
  measuring some evaluation metrics of its performance. 
}
\usage{
standardWF(form,train,test,
           learner,learner.pars=NULL,
           predictor='predict',predictor.pars=NULL,
           evaluator=if (is.factor(resp(form,train))) 'class.eval' else 'regr.eval',
           evaluator.pars=NULL,
           .outPreds=F)
}

\arguments{
  \item{form}{
    A formula specifying the predictive task.
  }
  \item{train}{
    A data frame containing the data set to be used for obtaining the
    predictive model (the training set).
  }
  \item{test}{
    A data frame containing the data set to be used for testing the
    obtained  model (the test set).
  }
  \item{learner}{
    A character string with the name of a function that is to be used to
  obtain the prediction models.
  }
  \item{learner.pars}{
    A list of parameter values to be passed to the learner (defaults to \code{NULL}).
  }
  \item{predictor}{
    A character string with the name of a function that is to be used to
  obtain the predictions for the test set using the obtained model
  (defaults to 'predict').
  }
  \item{predictor.pars}{
    A list of parameter values to be passed to the predictor (defaults
  to \code{NULL}).
  }
  \item{evaluator}{
    A character string with the name of a function that is to be used to
  obtain the evaluation metrics for the predictions of the model (if the
  target is a factor defaults to 'class.eval' else to 'regr.eval').
  }
  \item{evaluator.pars}{
    A list of parameter values to be passed to the evaluator (defaults
  to \code{NULL}).
  }
  \item{.outPreds}{
    A boolean that if set to \code{TRUE} will make the function return
  as an attribute of its result, a list with the true and predicted
  values (defaults  to \code{FALSE}).
  }
}
\details{
  The main goal of this function is to facilitate the task of the users
  of the experimental comparison infra-structure provided by function
  \code{\link{experimentalComparison}}. This latter function requires
  the user to write down a function implementing each of the workflows
  to be evaluated/compared by the infra-structure. Frequently, users
  just want to compare existing algorithms or variants of these algorithms on a
  set of predictive tasks, using some standard evaluation 
  metrics. In these situations, it may be tedious to have to write these
  functions. The goal of the \code{standardWF} function is to facilitate
  this task.

  Through parameter \code{learner} users may indicate the modeling
  algorithm to use to obtain the predictive model. This can be any R function,
  provided it can be called with a formula on the first argument and a
  training set on the second (as most R modeling functions do). As
  usual, these functions may include other arguments that are specific
  to the modeling approach (i.e. are parameters of the model). The
  values to be used for these parameters are specified as a list through
  the parameter \code{learner.pars} of function \code{standardWF}. The
  learning function can return any class of object that represents the
  learned model. This object will be used to obtain the predictions.

  Equivalently, the user may specify a function for obtaining the
  predictions for the test set using the previously learned model. Again
  this can be any function, and it is indicated in parameter
  \code{predictor} (defaulting to the usual \code{predict}
  function). This function should be prepared to accept in the first
  argument the learned model and in the second the test set, and should
  return the predictions of the model for this set of data. It may also
  have additional parameters whose values are specified as a list in
  parameter \code{predictor.pars}.

  Finally, the user may also specify a function that is to be used to
  obtain evaluation metrics using the predictions and true values of the
  test set. This can be any function which is specified through
  parameter \code{evaluator} (defaulting to \code{\link{class.eval}} for
  nominal target variables and \code{\link{regr.eval}} for numeric
  targets). The function will be called with the true values on the
  first argument and the predictions  on the
  second. Further arguments to this function are specified in the
  parameter \code{evaluator.pars} that should be a list of the values of
  these extra parameters of the evaluation function. The result of this
  evaluator function should be a named vector of the evaluation metrics,
  which will be the result returned by function \code{timeseriesWF}.

  In case the user sets the parameter \code{.outPreds} to \code{TRUE}
  the named vector with the evaluation metrics that is returned by the
  function will have an attribute named "itInfo" which will be a list
  with components \code{preds} and \code{trues}, with the predicted and
  true values, respectively, for the test set
}
\value{
  A named vector with the calculated statistics as returned by the
  function specified in parameter \code{evaluator}.
}
\references{ Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\note{
  In order to use any of the available classification algorithms you must
  have previously installed the respective packages if necessary.
}
\seealso{
 \code{\link{experimentalComparison}}, \code{\link{timeseriesWF}}, \code{\link{class.eval}}, \code{\link{regr.eval}}
}
\examples{
## Estimating the error rate on the Iris data set of three variants
## (different pruning levels) of a classification tree through 10-fold
## cross validation  
data(iris)
res <- experimentalComparison(
    c(dataset(Species ~ .,iris)),
    variants('standardWF',
             learner='rpartXse',learner.pars=list(se=c(0,0.5,1)),
             predictor.pars=list(type='class')
    ),
    cvSettings(1,10,123))

\dontrun{
  ## An example of a user defined workflow function. This one in
  ## particular will obtain a simple ensemble of a regression tree and a
  ## linear regression model, whose predictions are then combined using
  ## some weights. Finally, this wrokflow evaluates the model using the
  ## correlation between the true and predicted values
  ## We then use experimentalComparison to compare several variants of
  ## this user-defined workflow through bootstrap
  ##
  RLensemble <- function(f,tr,ts,weightRT=0.5,step=F,...) {
    noNAsTR <- knnImputation(tr)
    noNAsTS <- knnImputation(ts)
    r <- rpartXse(f,tr,...)
    l <- lm(f,noNAsTR)
    if (step) l <- step(l,trace=0)
    pr <- predict(r,ts)
    pl <- predict(l,noNAsTS)
    ps <- weightRT*pr+(1-weightRT)*pl
    c(correlation=cor(resp(f,ts),ps))
  }
  data(algae)
  expRes <- experimentalComparison(
  dataset(a1 ~ .,algae[,1:12],'alga1'),
  variants('RLensemble',
  se=c(0,1),step=c(T,F),weightRT=c(0.4,0.5,0.6)),
  bootSettings(1234,50))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
