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
    pre=NULL,pre.pars=NULL,
    post=NULL,post.pars=NULL,
    .fullOutput=FALSE)
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
  more information in the list that results from its execution like for
  instance  the  models (defaults  to \code{FALSE}).
  }
}
\details{
  The main goal of this function is to facilitate the task of the users
  of the experimental comparison infra-structure provided by function
  \code{\link{performanceEstimation}}. Namely, this function requires
  the users to specify the workflows (solutions to predictive tasks)
  whose performance she/he wants to estimate and compare. The user has
  the flexibility of writing hers/his own workflow functions, however,
  in most situations that is not really necessary. The reason is that
  most of the times users just want to compare standard out of the box
  learning algorithms on some tasks. In these contexts, the workflow
  simply consists of applying some existing learning algorithm to the
  training data, and then use it to obtain the predictions of the test
  set.  This standard workflow may even include some standard
  pre-processing tasks applied to the given data before the model is
  learned, and eventually some post processing tasks applied to the
  predictions before they are returned to the user. The goal of the
  current function is to facilitate evaluating this sort of estimation
  experiments. It implements this workflow thus avoiding the need of the
  user to write these workflows. 

  Through parameter \code{learner} users may indicate the modeling
  algorithm to use to obtain the predictive model. This can be any R function,
  provided it can be called with a formula on the first argument and a
  training set on the second (as most R modeling functions do). As
  usual, these functions may include other arguments that are specific
  to the modeling approach (i.e. are parameters of the model). The
  values to be used for these parameters are specified as a list through
  the parameter \code{learner.pars} of function \code{standardWF}. The
  learning function can return any class of object that represents the
  learned model. This object will be used to obtain the predictions in
  this standard workflow.

  Equivalently, the user may specify a function for obtaining the
  predictions for the test set using the previously learned model. Again
  this can be any function, and it is indicated in parameter
  \code{predictor} (defaulting to the usual \code{\link{predict}}
  function). This function should be prepared to accept in the first
  argument the learned model and in the second the test set, and should
  return the predictions of the model for this set of data. It may also
  have additional parameters whose values are specified as a list in
  parameter \code{predictor.pars}.

  Additionally, the user may specify a set of data-preprocessing
  functions to be applied to both the training and testing sets, through
  parameter \code{pre} that accepts a vector of function names. These
  functions will be applied to both the training and testing sets, in
  the sequence provided in the vector of names, before the learning
  algorithm is applied to the training set. Once again the user is free
  to indicate as pre-processing functions any function, eventually
  her/his own functions carrying our any sort of pre-processing
  steps. These user-defined pre-processing functions will be applied by
  function \code{\link{standardPRE}}. Check the help page of this
  function to know the protocol you need to follow to be able to use
  your own pre-processing functions. Still, our infra-structure already
  includes some common pre-processing functions so that you do not need
  to implement them. The list of these functions is again described in
  the help page of \code{\link{standardPRE}}.

  The predictions obtained by the function specified in parameter
  \code{predict} may also go through some post-processing steps before
  they are return as a result of the \code{standardWF} function. Again
  the user may specify a vector of post-processing functions to be
  applied in sequence, through the parameter \code{post}. Parameters to
  be passed to these functions can be specified through the parameter
  \code{post.pars}. The goal of these functions is to obtain a new
  version of the predictions of the models after going through some
  post-processing steps. These functions will be applied to the
  predictions by the function \code{\link{standardPOST}}. Once again
  this function already implements a few standard post-processing steps
  but you are free to supply your own post-processing functions provided
  they follow the protocol described in the help page of function
  \code{\link{standardPOST}}. 
  
  Finally, the parameter \code{.fullOutput} controls the ammount of
  information that is returned by the \code{standardWF} function. By
  default it is \code{FALSE} which means that the workflow will only
  return (apart from the predictions) the train, test and total times of
  the learning and prediction stages. This information is returned as a
  component named "times" of the results list that can be obtained
  for instance by using the
  \code{\link{getIterationsInfo}} if the workflow is being used in the
  context of an experimental comparison. If \code{.fullOutput} is set to
  \code{TRUE} the workflow will also include information on the
  pre-processing steps (in a component named "preprocessing"),
  information on the model and predictions of the model (in a component
  named "modeling") and information on the post-processing steps (in a
  component named "postprocessing").
  
}
\value{
  A list with several components containing the result of runing the
  workflow. 
}
\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\note{
  In order to use any of the available learning algorithms in R you must
  have previously installed and loaded the respective packages, if necessary.
}
\seealso{
  \code{\link{performanceEstimation}},
  \code{\link{timeseriesWF}},
  \code{\link{getIterationsInfo}},
  \code{\link{getIterationsPreds}},
  \code{\link{standardPRE}},
  \code{\link{standardPOST}}
}
\examples{
\dontrun{
data(iris)
library(e1071)

## a standard workflow using and SVM with default parameters
w.s <- Workflow(wfID="std.svm",learner="svm")
w.s

irisExp <- performanceEstimation(
  PredTask(Species ~ .,iris),
  w.s,
  EstimationTask("acc"))

getIterationsPreds(irisExp,1,1,it=4)
getIterationsInfo(irisExp,1,1,rep=1,fold=2)

## A more sophisticated standardWF
## - as pre-processing we imput NAs with either the median (numeric
## features) or the mode (nominal features); and we also scale
## (normalize) the numeric predictors
## - as learning algorithm we use and SVM with cost=10 and gamma=0.01
## - as post-processing we scale all predictions into the range [0..50]
w.s2 <- Workflow(pre=c("centralImp","scale"), 
                 learner="svm",
                 learner.pars=list(cost=10,gamma=0.01),
                 post="cast2int",
                 post.pars=list(infLim=0,supLim=50),
                 .fullOutput=TRUE
                )

data(algae,package="DMwR")

a1.res <- performanceEstimation(
            PredTask(a1 ~ ., algae[,1:12],"alga1"),
            w.s2,
            EstimationTask("mse")
            )

## Workflow variants of a standard workflow
ws <- workflowVariants(
                 pre=c("centralImp","scale"), 
                 learner="svm",
                 learner.pars=list(cost=c(1,5,10),gamma=0.01),
                 post="cast2int",
                 post.pars=list(infLim=0,supLim=c(10,50,80)),
                 .fullOutput=TRUE,
                 as.is="pre"
                )
a1.res <- performanceEstimation(
            PredTask(a1 ~ ., algae[,1:12],"alga1"),
            ws,
            EstimationTask("mse")
            )

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
