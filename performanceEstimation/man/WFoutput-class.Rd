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
  \code{times} with the traiun, test and total times of executing the
  workflow. Setting \code{fullOutput=TRUE} when creating the workflow
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

}
\keyword{classes}
