\name{timeseriesWF}
\alias{timeseriesWF}

\title{
  A function implementing sliding and growing window workflows for time series
  forecasting tasks
}
\description{
  This function implements sliding and growing window workflows for the
  prediction of both nominal and numerical time series. The sliding window workflow consists of: (i) starting by
  learning a prediction model based on the given training set, (ii) use
  this model to obtain predictions for a pre-defined number of future
  time steps; (iii) then  slide the training window forward this
  pre-defined number of steps and obtain a new model with this new
  training set; (iv) use this new model for obtaining another set of
  predictions; and (v) keep repeting this sliding process until
  predictions are obtained for all test set period.

  The growing window workflow is similar but instead of sliding the
  training window, we grow this window, so each new set of predictions
  is obtained with a model learned with all data since the beginning of
  the training set till the current time step. 
}
\usage{
timeseriesWF(form,train,test,
   learner,learner.pars=NULL,
   type='slide',relearn.step=1,
   predictor='predict',predictor.pars=NULL,
   evaluator=if (is.factor(responseValues(form,train))) 'classificationMetrics' 
             else 'timeseriesMetrics',
   evaluator.pars=NULL,
   .outPreds=TRUE,.outModels=FALSE,
   verbose=T)
}

\arguments{
  \item{form}{
    A formula specifying the predictive task.
  }
  \item{train}{
    A data frame containing the data set to be used for obtaining the
    first prediction model. In case we are using the sliding window
  approach, the size of this training set will determine the size of all
  future  training sets after each slide step.
  }
  \item{test}{
    A data frame containing the data set for which we want predictions.
  }
  \item{learner}{
    A character string with the name of a function that is to be used to
  obtain the prediction models.
  }
  \item{learner.pars}{
    A list of parameter values to be passed to the learner (defaults to \code{NULL}).
  }
  \item{type}{
    A character string specifying if we are using a sliding (value
    'slide') or a growing (value 'grow') window workflow (defaults to 'slide'). 
  }
  \item{relearn.step}{
    The number of time steps (translated into number of rows in the test
    set) after which a new model is re-learned (either by sliding or
    growing the training window) (defaults to 1, i.e. each new row).
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
  target is a factor defaults to 'class.eval' else to 'ts.eval').
  }
  \item{evaluator.pars}{
    A list of parameter values to be passed to the evaluator (defaults
  to \code{NULL}).
  }
  \item{.outPreds}{
    A boolean indicating whether the function should include the
  predictions of the workflow in the \code{\linkS4class{WFoutput}}
  object that is returned (defaults to \code{TRUE})
  }
  \item{.outModels}{
    A boolean indicating whether the function should include the
  models learned by the workflow in the \code{\linkS4class{WFoutput}}
  object that is returned (defaults to \code{FALSE})
  }
  \item{verbose}{
    A Boolean indicating whether a "*" character should be printed every
    time the window slides (defaults to TRUE).
  }
}
\details{
  The main goal of this function is to facilitate the task of the users
  of the experimental comparison infra-structure provided by function
  \code{\link{performanceEstimation}} for time series problems where
  the target variable can be numeric or nominal. This latter function requires
  the user to write down a function implementing each of the workflows
  to be evaluated/compared by the infra-structure through a Monte Carlo
  simulation process, which is the experimental methodology recommended
  for time series problems in this infra-structure. Frequently, users
  just want to compare existing algorithms or variants of these algorithms on a
  set of forecasting tasks, using some standard  error
  metrics. In these situations, it may be tedious  having to write these
  functions. The goal of the \code{timeseriesWF} function is to facilitate
  this task.

  We have considered two typical workflow approaches for time series
  tasks where the user wants predictions for a certain future time
  period (a test set provided as a data frame in parameter
  \code{test}). Both are based on the assumption that after "some" time
  the model that we have obtained with the given training period data
  (parameter \code{train}) might have become out-dated, and thus a new
  model should be obtained with the most recent data. The idea is that
  as we move in the testing period and get predictions for the successive
  rows of the test set, it is like a clock is advancing. Previous rows
  for which we already got a prediction are "past" as we assume that the
  successive rows in both the \code{train} and \code{test} data frames
  are ordered by time (they are time series). In this context, as we
  move forward in the test period we can regard the rows for which we
  already got a prediction as past data, and thus potentially useful to be
  added  to our training set with the goal of obtaining a fresh new
  model with more recent data. This type of reasoning only makes sense
  if we suspect that there is some kind of concept drift on our
  data. For stationary data this makes no sense and we would be better
  off using the workflow provided by function
  \code{\link{standardWF}}. Still, the current function implements two
  workflows following this model-updating reasoning: (i) sliding window;
  and (ii) growing window. Both have as user-defined parameter
  (\code{relearn.step}) the number of time periods after which we re-learn
  the model using fresh new data. The difference between them lies on
  how they treat the oldest data (the initial rows of the provided
  training set). Sliding window, as the name suggests, after each
  relearn step slides the training set forward thus forgetting the
  oldest rows of the previous training set whilst incorporating the most
  recent observations. With this approach all models are obtained with a
  training set with the same amount of data (the number of rows of the
  initially given training set). Growing window does not remove older
  rows and thus the training sets keep growing in size after each
  relearn step.

  Independently of the process used to obtain the predictions for the
  test set, these are based on a prediction model. Through parameter
  \code{learner} users may indicate the modeling algorithm to use to
  obtain this model. This can be any  R function, provided it can be
  called with a formula on the first argument and a training set on the
  second (as most R modeling functions do). As usual, these functions
  may include other arguments that are specific to the modeling approach
  (i.e. are parameters of the model). The values to be used for these
  parameters are specified as a list through the parameter
  \code{learner.pars} of function \code{timeseriesWF}. The learning
  function can return any class of object that represents the learned
  model. This object will be used to  obtain the predictions.

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
  parameter \code{evaluator} (defaulting to \code{\link{classificationMetrics}} for
  nominal target variables and \code{\link{timeseriesMetrics}} for numeric
  targets). The function will be called with the true values on the
  first argument and the predictions  on the
  second. Further arguments are supplied next, as specified in the
  parameter \code{evaluator.pars} that should be a list of the values of
  these extra parameters of the evaluation function. The result of this
  evaluator function should be a named vector of the evaluation metrics,
  which will be the result returned by function \code{timeseriesWF}.
}
\value{
  A named vector with the calculated statistics as returned by the
  function specified in parameter \code{evaluator}.
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
 \code{\link{standardWF}},  \code{\link{performanceEstimation}},
 \code{\link{classificationMetrics}},  \code{\link{timeseriesMetrics}}
}
\examples{

## The following is a small illustrative example using the quotes of the
## SP500 index. This example compares two random forests with 500
## regression trees, one applyed in a standard way, and the other using
## a growing window with a relearn step of every 5 days. The experiment
## uses 10 repetitions of a train+test cycle using 50\% of the available
## data for training and 25\% for testing.
\dontrun{
library(quantmod)
library(randomForest)
getSymbols('^GSPC',from='2008-01-01',to='2012-12-31')
data.model <- specifyModel(
  Next(100*Delt(Ad(GSPC))) ~ Delt(Ad(GSPC),k=1:10)+Delt(Vo(GSPC),k=1:3))
data <- modelData(data.model)
colnames(data)[1] <- 'PercVarClose'
spExp <- performanceEstimation(
  PredTask(PercVarClose ~ .,data,'SP500_2012'),
  c(Workflow('standardWF',wfID="standRF",
             learner='randomForest',
             learner.pars=list(ntree=500)),
    Workflow('timeseriesWF',efID,"slideRF",
             learner='randomForest',
             learner.pars=list(ntree=500,relearn.step=5))
   ),
  McSettings(10,0.5,0.25))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
