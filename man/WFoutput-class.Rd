\name{WFoutput-class}
\Rdversion{1.1}
\docType{class}
\alias{WFoutput}
\alias{WFoutput-class}
\alias{show,WFoutput-method}
\alias{summary,WFoutput-method}

\title{Class "WFoutput"}

\description{
  Objects of the class \code{WFoutput} store the result of runing a
  workflow on a predictive task. These results, in the context of our
  performance estimation framework, are formed by information on the
  predictions of the workflow and some extra information that the
  workflow author may have decided to return.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{WFoutput(
  ...)}. This constructor accepts in the first argument the row ids of
  the test cases, in the second the true values of the target variable
  of these test cases, and in the third argument the predictions of the
  workflow for these test cases. Note that these predictions may
  eventually be a data frame or matrix, which is the case of probabilistic
  classifiers that obtain probabilities of each class for each test
  case. In this situations the constructor will automatically calculate
  (and include in the resulting object) an extra column with the label
  of the class with highest probability. Optionally, in the fourth argument you
  may include a list with any extra information you wish to return from
  the workflow.  This later information is often set using the
   function \code{\link{workflowInformation}}.
}

\section{Slots}{
  \describe{
    \item{\code{predictions}:}{A \code{data.frame} with at least two columns, the first with
  the true values of the target variable of all  test cases and the
  second with the predicted values for these cases. In case of
  probabilistic classifiers it may also contain as many further columns
  as there are class labels, each with the probability of the respective
  class label for the test case in the respective row. The row names of
  this data.frame are the test cases IDs.}
    \item{\code{extraInfo}:}{A list with extra information returned from
  the execution of the workflow. This is controled by the workflow
  function. The standard workflow will at least return a component named
  \code{times} with the train, test and total times of executing the
  workflow. Setting \code{.fullOutput=TRUE} when creating the workflow
  will generate several other components with information from the
  different steps of the standard workflow.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "WFoutput")}: method used to
      show the contents of a \code{WFoutput} object. }

  \item{summary}{\code{signature(object = "WFoutput")}: method used to
    obtain a summary of the contents of a \code{WFoutput} object. }
    }
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
  \code{\linkS4class{Workflow}},
  \code{\link{workflowInformation}},
  \code{\link{workflowPredictions}},
  \code{\link{runWorkflow}},
  \code{\link{standardWF}},
  \code{\link{timeseriesWF}}
}
\examples{
showClass("WFoutput")

## A user-defined workflow returning a WFoutput object
myWF <- function(form,train,test,wL=0.5,...) {
    require(rpart,quietly=TRUE)
    ml <- lm(form,train)
    mr <- rpart(form,train)
    pl <- predict(ml,test)
    pr <- predict(mr,test)
    ps <- wL*pl+(1-wL)*pr
    res <- WFoutput(rownames(test),responseValues(form,test),ps)
    workflowInformation(res) <-
      list(models=list(lm=ml,tree=mr),indivPreds=list(lm=pl,tree=pr))
    res
}
}
\keyword{classes}
