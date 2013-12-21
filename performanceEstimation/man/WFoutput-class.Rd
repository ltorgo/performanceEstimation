\name{WFoutput-class}
\Rdversion{1.1}
\docType{class}
\alias{WFoutput}
\alias{WFoutput-class}
\alias{show,WFoutput-method}

\title{Class "WFoutput"}

\description{
  Objects of the class \code{WFoutput} store the result of runing a
  workflow on a predictive task. These results, in the context of our
  performance estimation framework, are formed by at least the scores
  of a set of evaluation metrics resulting from applying the workflow and also
  some other optional information. Namely, they may also contain a
  matrix with the true and predicted values for each test case, and also
  a list with any other information the creator of the workflow deems
  important to return (e.g. the model obtained with the training set).
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{WFoutput( ...)}
  providing the values for the class slots.  
}

\section{Slots}{
  \describe{
    \item{\code{scores}:}{A named vector with the scores of the metrics
  being estimated.}
    \item{\code{predictions}:}{A matrix with two columns, the first with
  the true values of the target variable of all  test cases and the
  second with the predicted values for these cases. This slot is
  optional and defaults to an empty matrix.}
    \item{\code{extraInfo}:}{An optional list (defaulting to the empty
  list) containing any other information the creator of the workflow
  deems as important to return (e.g. the model obtained with the
  training data).}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "WFoutput")}: method used to
      show the contents of a WFoutput object. }
	 }
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{\code{\linkS4class{Workflow}},  \code{\link{workflowInformation}},  \code{\link{workflowPredictions}}, \code{\link{runWorkflow}}
}
\examples{
showClass("WFoutput")

}
\keyword{classes}
