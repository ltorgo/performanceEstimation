\name{standardPOST}
\alias{standardPOST}

\title{
A function for applying post-processing steps to the predictions of a model
}
\description{
  This function implements a series of simple post-processing steps
  to be applied to the predictions of a model. It also allows the user
  to supply hers/his own post-processing functions. The result of the
  function is a new version of the predictions of the model (typically a
  vector or a matrix in the case of models that predict class
  probabilities, for instance).
}
\usage{
standardPOST(form, train, test, preds, steps, ...)
}

\arguments{
  \item{form}{
    A formula specifying the predictive task.
}
  \item{train}{
    A data frame containing the training set.
}
  \item{test}{
    A data frame containing the test set.
  }
  \item{preds}{
   The object resulting from the application of a model to the test set
  to obtain its predictions (typically a vector or a matrix for
  probabilistic classifiers) 
  }
  \item{steps}{
    A vector with function names that are to be applied in the sequence
    they appear in this vector to the predictions to obtain a new
  version of these predictions.
}
  \item{\dots}{
    Any further parameters that will be passed to all functions
    specified in \code{steps}
}
}
\details{
  This function is mainly used by both \code{\link{standardWF}} and
  \code{\link{timeseriesWF}} as a means to allow for users of these two
  standard workflows to specify some post-processing steps for the
  predictions of the models. These
  are steps one wishes to apply to the predictions to somehow change the
  outcome of the prediction stage.

  Nevertheless, the function can also be used outside of these standard
  workflows for obtaining post-processed versions of the predictions.

  The function accepts as post-processing functions both some already
  implemented functions as well as any function defined by the user
  provided these follow some protocol. Namely, these user-defined
  post-processing functions should be aware that they will be called with
  a formula, a training data frame, a testing data frame and the
  predictions in the first
  four arguments. Moreover, any arguments used in the call to
  \code{standardPOST} will also be forwarded to these user-defined
  functions. Finally, these functions should return a new version of the
  predictions. It is questionable the interest of supplying both the
  training and test sets to these functions, on top of the formula and
  the predictions. However, we have decided to pass them anyway not to
  precule the usage of any special post-processing step that requires
  this information. 

  The function already contains implementations of the following
  post-processing steps that can be used in the \code{steps} parameter:

  "na2central" - this function fills in any \code{NA} predictions into
  either the median (numeric targets) or mode (nominal targets) of the
  target variable on the training set. Note that this is only applicable
  to predictions that are vectors of values.

  "onlyPos" - in some numeric forecasting tasks the target variable
  takes only positive values. Nevertheless, some models may insist in
  forecasting negative values. This function casts these negative values
  to zero. Note that this is only applicable
  to predictions that are vectors of numeric values.

  "cast2int" - in some numeric forecasting tasks the target variable
  takes only values within some interval. Nevertheless, some models may insist in
  forecasting  values outside of this interval. This function casts these values
  into the nearest interval boundary. This function requires that you
  supply the limits of this interval through parameters \code{infLim}
  and \code{supLim}. Note that this is only applicable
  to predictions that are vectors of numeric values.

}
\value{
  An object of the same class as the input parameter \code{preds}
}
\references{Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\link{standardPRE}},
  \code{\link{standardWF}},
  \code{\link{timeseriesWF}}

}
\examples{
\dontrun{

######
## Using in the context of an experiment


data(algae,package="DMwR")
library(e1071)

## This will issue several warnings because this implementation of SVMs
## will ignore test cases with NAs in some predictor. Our infra-structure
## issues a warning and fills in these with the prediction of an NA
res <- performanceEstimation(
  PredTask(a1 ~ .,algae[,1:12],"alga1"),
  Workflow(learner="svm"),
  EstimationTask(metrics="mse")
  )
summary(getIterationPreds(res,1,1,it=1))

## one way of overcoming this would be to post-process the NA
## predictions into a statistic of centrality
resN <- performanceEstimation(
  PredTask(a1 ~ .,algae[,1:12],"alga1"),
  Workflow(learner="svm",post="na2central"),
  EstimationTask(metrics="mse")
  )
summary(getIterationPreds(resN,1,1,it=1))

## because the SVM also predicts negative values which does not make
## sense in this application (the target are frequencies thus >= 0) we
## could also include some further post-processing to take care of
## negative predictions
resN <- performanceEstimation(
  PredTask(a1 ~ .,algae[,1:12],"alga1"),
  Workflow(learner="svm",post=c("na2central","onlyPos")),
  EstimationTask(metrics="mse")
  )
summary(getIterationPreds(resN,1,1,it=1))

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models}

