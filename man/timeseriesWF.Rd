\name{timeseriesWF}
\alias{timeseriesWF}

\title{
  A function implementing sliding and growing window standard workflows for time series
  forecasting tasks
}
\description{
  This function implements sliding and growing window workflows for the
  prediction  time series. The sliding window workflow consists of: (i) starting by
  learning a prediction model based on the given training set, (ii) use
  this model to obtain predictions for a pre-defined number of future
  time steps of the test set; (iii) then  slide the training window forward this
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
   pre=NULL,pre.pars=NULL,
   post=NULL,post.pars=NULL,
   .fullOutput=FALSE,verbose=FALSE)
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
  \item{pre}{
A vector of function names that will be applied in sequence to the train
  and test data frames, generating new versions, i.e. a sequence of data
  pre-processing functions.
  }
  \item{pre.pars}{
A named list of parameter values to be passed to the pre-processing functions.
  }
  \item{post}{
A vector of function names that will be applied in sequence to the
  predictions of the model, generating a new version, i.e. a sequence of data
  post-processing functions.

  }
  \item{post.pars}{
A named list of parameter values to be passed to the post-processing functions.
  }
  \item{.fullOutput}{
    A boolean that if set to \code{TRUE} will make the function return
  the models that was obtained in the extra information of the workflow
  output object (you may inspect it using function \code{\link{workflowInformation}}) (defaults  to \code{FALSE}).
  }
  \item{verbose}{
    A Boolean indicating whether a "*" character should be printed every
    time the window slides (defaults to \code{FALSE}).
  }
}
\details{
  The main goal of this function is to facilitate the task of the users
  of the experimental comparison infra-structure provided by function
  \code{\link{performanceEstimation}} for time series problems where
  the target variable can be numeric or nominal.  Frequently, users
  just want to compare existing algorithms or variants of these algorithms on a
  set of forecasting tasks, using some standard  error
  metrics.  The goal of the \code{timeseriesWF} function is to facilitate
  this task by providing a standard workflow for time series tasks.

  The function works, and has almost the same parameters, as function
  \code{\link{standardWF}}. The help page of this latter function
  describes most of the parameters used in the current function and thus
  we will not repeat the description here. The main difference to the
  \code{\link{standardWF}} function is on the existance of two extra
  parameters that control the sliding and growing window approaches to
  time series forecasting. These are parameters \code{type} and
  \code{relearn.step}.  We have considered two typical workflow approaches for time series
  tasks where the user wants predictions for a certain future time
  period. Both are based on the assumption that after "some" time
  the model that we have obtained with the given training period data
   may have become out-dated, and thus a new
  model should be obtained with the most recent data. The idea is that
  as we move in the testing period and get predictions for the successive
  rows of the test set, it is like if a clock is advancing. Previous rows
  for which we already made a prediction are "past" as we assume that the
  successive rows in both the \code{train} and \code{test} data frames
  are ordered by time (they are time series). In this context, as we
  move forward in the test period we can regard the rows for which we
  already made a prediction as past data, and thus potentially useful to be
  added  to our initial training set with the goal of obtaining a fresh new
  model with more recent data. This type of reasoning only makes sense
  if we suspect that there is some kind of concept drift on our
  data. For stationary data this makes no sense and we would be better
  off using the workflow provided by function
  \code{\link{standardWF}}. Still, the current function implements two
  workflows following this model-updating reasoning: (i) sliding window;
  and (ii) growing window. Both use the value of the parameter
  (\code{relearn.step}) to decide the number of time periods after which we re-learn
  the model using fresh new data. The difference between the two strategies lies on
  how they treat the oldest data (the initial rows of the provided
  training set). Sliding window, as the name suggests, after each
  relearn step slides the training set forward thus forgetting the
  oldest rows of the previous training set whilst incorporating the most
  recent observations. With this approach all models are obtained with a
  training set with the same amount of data (the number of rows of the
  initially given training set). Growing window does not remove older
  rows and thus the training sets keep growing in size after each
  relearn step.

}
\value{
  An object of class \code{\linkS4class{WFoutput}}
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\link{standardWF}},
  \code{\link{performanceEstimation}},
  \code{\link{workflowPredictions}},
  \code{\link{workflowInformation}},
  \code{\link{getIterationInfo}},
  \code{\link{getIterationPreds}},
  \code{\link{standardPRE}},
  \code{\link{standardPOST}},
  \code{\linkS4class{WFoutput}}
}
\examples{

## The following is a small illustrative example using the quotes of the
## SP500 index. This example compares two random forests with 500
## regression trees, one applyed in a standard way, and the other using
## a sliding window with a relearn step of every 10 days. The experiment
## uses 10 repetitions of a train+test cycle using 50\% of the available
## data for training and 25\% for testing.
\dontrun{
library(quantmod)
library(randomForest)
getSymbols('^GSPC',from='2008-01-01',to='2012-12-31')
data.model <- specifyModel(
  Next(100*Delt(Ad(GSPC))) ~ Delt(Ad(GSPC),k=1:10)+Delt(Vo(GSPC),k=1:3))
data <- as.data.frame(modelData(data.model))
colnames(data)[1] <- 'PercVarClose'
spExp <- performanceEstimation(
  PredTask(PercVarClose ~ .,data,'SP500_2012'),
  c(Workflow(wf='standardWF',wfID="standRF",
             learner='randomForest',
             learner.pars=list(ntree=500)),
    Workflow(wf='timeseriesWF',wfID="slideRF",
             learner='randomForest',
             learner.pars=list(ntree=500),
             type="slide",
             relearn.step=10)
   ),
  EstimationTask(
     metrics=c("mse","theil"),
     method=MonteCarlo(nReps=5,szTrain=0.5,szTest=0.25)
     )
)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
