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
\code{estTask} (check the help page of \code{\linkS4class{MonteCarlo}}).

Please note that most of the times you will not call this function
directly, though there is nothing wrong in doing it, but instead you
will use the function \code{\link{performanceEstimation}}, that allows you to
carry out performance estimation for multiple workflows on multiple tasks,
using some estimation method. Still, when you
simply want to have the Monte Carlo estimates for one workflow on one task,
you may prefer to use this function directly.  
}

\usage{
mcEstimates(wf, task, estTask, verbose = TRUE,cluster)
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
  \item{estTask}{
    an object of the class \code{\linkS4class{EstimationTask}} indicating the metrics to
    be estimated and the Monte Carlo settings to use.
  }
  \item{verbose}{
    A boolean value controlling the level of output of the function
    execution, defaulting to \code{TRUE}
  }
   \item{cluster}{
    an optional parameter that can either be \code{TRUE} or a
    \code{\link{cluster}}. In case of \code{TRUE} the function will run in
    parallel and will internally setup the parallel back-end (defaulting
    to using half of the cores in your local machine). You may also setup
    outside your parallel back-end (c.f. \code{\link{makeCluster}}) and
    then pass the resulting \code{cluster} object to this function using
    this parameter. In case no value is provided for this parameter the
    function will run sequentially.
  }
 
}

\details{
  This function provides reliable estimates of a set of evaluation
  statistics through a Monte Carlo experiment. The user supplies a worflow
  function and a data set of a time series forecasting task, together
  with the estimation task. This task should include both the metrics to
  be estimated as well as the settings of the estimation methodology
  (MOnte Carlo) that include,
  among others, the size of the training (TR) and testing sets (TS) and
  the number 
  of repetitions (R) of the train+test cycle. The function randomly selects
  a set of R numbers in the time interval [TR+1,NDS-TS+1], where NDS is the
  size of the full data set. For each of these R numbers the previous TR
  observations of the data set are used to learn a model and the
  subsequent TS observations for testing it and obtaining the wanted
  evaluation metrics. The resulting R estimates of the evaluation
  metrics are averaged at the end of this process resulting in the
  Monte Carlo estimates of these metrics.
  
  This function is targeted at obtaining estimates of
  performance for time series prediction problems. The reason is that
  the experimental repetitions ensure that the order of the rows in the
  original data set are never swaped, as these rows are assumed to be
  ordered by time. This is an important issue to
  ensure that a prediction model is never tested on past observations of
  the time series.

  For each train+test iteration the provided workflow function is called
  and should return the predictions  of the workflow for the given test
  period. To carry out this train+test iteration the user may use the
  standard time series workflow that is provided (check the help page of
  \code{\link{timeseriesWF}}), or may provide hers/his own workflow that
  should return an object of class
  \code{\linkS4class{WFoutput}}. See the Examples section below for an
  example of these functions. Further examples are given in the package
  vignette.

  Parallel execution of the estimation experiment is only recommended
  for minimally large data sets otherwise you may actually increase the
  computation time due to communication costs between the processes.
  
}

\value{
  The result of the function is an object of class \code{EstimationResults}.
}

\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
  \code{\linkS4class{MonteCarlo}},
  \code{\linkS4class{Workflow}},
  \code{\link{timeseriesWF}},
  \code{\linkS4class{PredTask}},
  \code{\linkS4class{EstimationTask}},
  \code{\link{performanceEstimation}},
  \code{\link{hldEstimates}},
  \code{\link{loocvEstimates}},  
  \code{\link{cvEstimates}},
  \code{\link{bootEstimates}},
  \code{\linkS4class{EstimationResults}}
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
                              type="slide",relearn.step=10,
                              learner='randomForest'),
                    PredTask(PercVarClose ~ .,sp500.data,"sp500"),
                    EstimationTask(metrics=c("mse","theil"),
                                   method=MonteCarlo(nReps=4,szTrain=.3,szTest=.2)))

summary(spExp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

