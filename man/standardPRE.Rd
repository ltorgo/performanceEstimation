\name{standardPRE}
\alias{standardPRE}

\title{
A function for applying data pre-processing steps
}
\description{
  This function implements a series of simple data pre-processing steps
  and also allows the user to supply hers/his own functions to be
  applied to the data. The result of the function is a list containing
  the new (pre-processed) versions of the given train and test sets.
}
\usage{
standardPRE(form, train, test, steps, ...)
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
  \item{steps}{
    A vector with function names that are to be applied in the sequence
    they appear in this vector to both the training and testing sets, to
    obtain new versions of these two data samples.
}
  \item{\dots}{
    Any further parameters that will be passed to all functions
    specified in \code{steps}
}
}
\details{
  This function is mainly used by both \code{\link{standardWF}} and
  \code{\link{timeseriesWF}} as a means to allow for users of these two
  standard workflows to specify some data pre-processing steps. These
  are steps one wishes to apply to the different train and test samples
  involved in an experimental comparison, before any model is learned or
  any predictions are obtained.

  Nevertheless, the function can also be used outside of these standard
  workflows for obtaining pre-processed versions of train and test
  samples.

  The function accepts as pre-processing functions both some already
  implemented functions as well as any function defined by the user
  provided these follow some protocol. Namely, these user-defined
  pre-processing functions should be aware that they will be called with
  a formula, a training data frame and a testing data frame in the first
  three arguments. Moreover, any arguments used in the call to
  \code{standardPRE} will also be forwarded to these user-defined
  functions. Finally, these functions should return a list with two
  components: "train" and "test", containing the pre-processed versions
  of the supplied train and test data frames.

  The function already contains implementations of the following
  pre-processing steps that can be used in the \code{steps} parameter:

  "scale" - that scales (subtracts the mean and divides by the standard
  deviation) any knnumeric features on both the training and testing
  sets. Note that the mean and standard deviation are calculated using
  only the training sample.

  "centralImp" - that fills in any \code{NA} values in both sets using
  the median value for numeric predictors and the mode for nominal
  predictors. Once again these centrality statistics are calculated
  using only the training set although they are applied to both train
  and test sets.

  "na.omit" - that uses the R function \code{\link{na.omit}} to remove
  any rows containing \code{NA}'s  from both the training and test sets.

  "undersampl" - this undersamples the training data cases that do not
  belong to the minority class (this pre-processing step is only
  available for classification tasks!). It takes the parameter
  \code{perc.under} that controls the level of undersampling
  (defaulting to 1, which means that there would be as many cases from
  the minority as from the other(s) class(es)).

  "smote" - this operation uses the SMOTE (Chawla et. al. 2002)
  resampling algorithm to  generate a new training sample with a more
  "balanced" distributions of the target class (this pre-processing step
  is only available for classification tasks!). It takes the parameters
  \code{perc.under},  \code{perc.over} and \code{k} to control the
  algorithm. Read the  documentation of function \code{\link{smote}} to
  know more details. 
}
\value{
  A list with components "train" and "test" with both containing a data frame.
}
\references{
  Chawla, N. V., Bowyer, K. W., Hall, L. O., and Kegelmeyer, W. P. (2002).
  \emph{Smote: Synthetic minority over-sampling technique}. Journal of Artificial
  Intelligence Research, 16:321-357.
  
  Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
  \code{\link{standardPOST}},
  \code{\link{standardWF}},
  \code{\link{timeseriesWF}}

}
\examples{
\dontrun{

##  A small example with standard pre-preprocessing: clean NAs and scale
data(algae,package="DMwR")

idx <- sample(1:nrow(algae),150)
tr <- algae[idx,1:12]
ts <- algae[-idx,1:12]
summary(tr)
summary(ts)

preData <- standardPRE(a1 ~ ., tr, ts, steps=c("centralImp","scale"))
summary(preData$train)
summary(preData$test)

######
## Using in the context of an experiment
library(e1071)
res <- performanceEstimation(
  PredTask(a1 ~ .,algae[,1:12],"alga1"),
  Workflow(learner="svm",pre=c("centralImp","scale")),
  EstimationTask(metrics="mse")
  )

summary(res)

######
## A user-defined pre-processing function
myScale <- function(f,tr,ts,avg,std,...) {
    tgtVar <- deparse(f[[2]])
    allPreds <- setdiff(colnames(tr),tgtVar)
    numPreds <- allPreds[sapply(allPreds,
                          function(p) is.numeric(tr[[p]]))]
    tr[,numPreds] <- scale(tr[,numPreds],center=avg,scale=std)
    ts[,numPreds] <- scale(ts[,numPreds],center=avg,scale=std)
    list(train=tr,test=ts)
}

## now using it with some random averages and stds for the 8 numeric
## predictors (just for illustration)
newData <- standardPRE(a1 ~ .,tr,ts,steps="myScale",
                       avg=rnorm(8),std=rnorm(8))

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models}

