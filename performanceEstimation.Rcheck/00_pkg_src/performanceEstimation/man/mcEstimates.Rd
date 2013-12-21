\name{mcEstimates}
\alias{mcEstimates}

\title{
Performance estimation for time series prediction tasks using Monte Carlo

}
\description{
  This function performs a Monte Carlo experiment with the goal of
  estimating the performance of a given approach (a workflow) on a
  certain time  series prediction task. The function is general in the
  sense that the workflow function that the user provides as the
  solution to the task,  can implement or call whatever modeling
  technique the user wants. 

The function implements Monte Carlo estimation and different settings
concerning this methodology are available through the argument
\code{sets} (check the help page of \code{\linkS4class{McSettings}}).

Please note that most of the times you will not call this function
directly, though there is nothing wrong in doing it, but instead you
will use the function \code{\link{performanceEstimation}}, that allows you to
carry out performance estimation for multiple workflows on multiple tasks,
using some estimation method. Still, when you
simply want to have the Monte Carlo estimates for one workflow on one task,
you may prefer to use this function directly.  
}

\usage{
mcEstimates(wf, task, mcSet, verbose = TRUE)
}


\arguments{
  \item{wf}{
an object of the class \code{Workflow} representing the
  modeling approach to be evaluated on a certain task.
}
  \item{task}{
an object of the class \code{PredTask} representing the
  prediction task to be used in the evaluation.
}
  \item{mcSet}{
an object of the class \code{McSettings} representing the
  Monte Carlo settings to use.
}
  \item{verbose}{
    A boolean value controlling the level of output of the function
    execution, defaulting to \code{TRUE}
  }
}

\details{
  This function provides reliable estimates of a set of evaluation
  statistics through a Monte Carlo experiment. The user supplies a worflow
  function and a data set of a time series forecasting task, together
  with the experiment settings.  These settings should specify,
  among others, the size of the training (TR) and testing sets (TS) and
  the number 
  of repetitions (R) of the train+test cycle. The function randomly selects
  a set of R numbers in the time interval [TR+1,NDS-TS+1], where NDS is the
  size of the full data set. For each of these R numbers the previous TR
  observations of the data set are used to learn a model and the
  subsequent TS observations for testing it and obtaining the wanted
  evaluation statistics. The resulting R estimates of the evaluation
  statistics are averaged at the end of this process resulting in the
  Monte Carlo estimates of these metrics.
  
  This function is targeted at obtaining estimates of
  performance for time series prediction problems. The reason is that
  the experimental repetitions ensure that the order of the rows in the
  original data set are never swaped, as these rows are assumed to be
  ordered by time. This is an important issue to
  ensure that a prediction model is never tested on past observations of
  the time series.

  For each train+test iteration the provided workflow function is called
  and should return the results of the workflow in terms of the
  statistics for which Monte Carlo estimates are required. It is the
  user responsibility to decide which statistics are to be evaluated on
  each iteration and how they are calculated. This is done by creating a
  function that the user knows it will be called by this Monte Carlo
  routine at each iteration of the process. This user-defined workflow
  function must assume that it will receive in the first 3 arguments a
  formula, a training set and a testing set, respectively. It should
  also assume that it may receive any other set of parameters that
  should be passed towards the modeling algorithm. The result of this
  user-defined function should be an object of class
  \code{\linkS4class{WFoutput}}. See the Examples section below for an
  example of these functions. Further examples are given in the package
  vignette.
  
  To facilitate the task of creating these user-defined workflow
  functions we provide the \code{\link{timeseriesWF}} function that can
  be used as a kind of skeleton of a general purpose workflow for time
  series predictive tasks where the user simply indicates a few small
  details (e.g. the learning algorithm to use). Check the help page of
  this function for more details and examples. In the Examples section
  of the current function you will also find some illustrations of the
  use of this function.
 
}

\value{
  The result of the function is an object of class \code{EstimationResults}.
}

\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
\code{\link{performanceEstimation}}, \code{\link{timeseriesWF}},
\code{\linkS4class{EstimationResults}},\code{\linkS4class{McSettings}},
\code{\link{bootEstimates}}, \code{\link{hldEstimates}}, \code{\link{loocvEstimates}},
\code{\link{cvEstimates}}
}

\examples{

## The following is a small illustrative example using the quotes of the
## SP500 index. This example estimates the performance of a random
## forest on a illustrative example of trying to forecast the future
## variations of the adijusted close prices of the SP500 using a few
## predictors. The random forest is evaluated on 4 repetitions of a
## monte carlo experiment where 30\% of the data is used for training
## the model that is then used to make predictions for the next 20\%,
## using a sliding window approach with a relearn step of 10 periods
## (check the help page of the timeseriesWF() function to understand
## these and other settings)

\dontrun{
library(quantmod)
library(randomForest)

getSymbols('^GSPC',from='2008-01-01',to='2012-12-31')
data.model <- specifyModel(Next(100*Delt(Ad(GSPC))) ~ Delt(Ad(GSPC),k=1:10)+Delt(Vo(GSPC),k=1:3))
data <- as.data.frame(modelData(data.model))
colnames(data)[1] <- 'PercVarClose'

spExp <- mcEstimates(Workflow("timeseriesWF",wfID="rfTrial",
                             relearn.step=10,learner='randomForest'),
                    PredTask(PercVarClose ~ .,data,"sp500"),
                    McSettings(4,.3,.2))

summary(spExp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

