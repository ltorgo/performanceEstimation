\name{Workflow-class}
\Rdversion{1.1}
\docType{class}
\alias{Workflow}
\alias{Workflow-class}
\alias{show,Workflow-method}
\alias{summary,Workflow-method}

\title{Class "Workflow"}

\description{
  Objects of the class \code{Workflow} represent a
  solution to a predictive task that typically involve learning a
  prediction model using the given training data and then applying it to
  the provided test set. Still, a workflow function may carry out many
  more steps (e.g. special data pre-processing steps, or some form of
  post-processing of the predictions, etc.)
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{Workflow( ...)}.
  The objects contain information on the R function implementing
  the solution to predictive tasks provided by the workflow, and also
  a list of arguments with  respective values, that should be used when
  calling that function. This constructor function also accepts an
  argument named \code{wfID} if you wish to give a particular name to
  your workflow (it defaulrs to the name of the function indicated as
  implementing the workflow, the first argument to the
  constructor). Check the Examples section below.
}

\section{Slots}{
  \describe{
    \item{\code{func}:}{A character string containing the name of the R
      function that implements the workflow.}
    \item{\code{pars}:}{A named list containing the parameters and
      respective values to be used when calling the workflow (defaulting
      to the empty list).}
    \item{\code{name}:}{An optional string containing an
    internal name of the workflow (a kind of ID)}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Workflow")}: method used to
      show the contents of a \code{Workflow} object. }

  \item{summary}{\code{signature(object = "Workflow")}: method used to
    obtain a summary of a \code{Workflow} object. }
  }
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison on Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{\code{\linkS4class{PredTask}},  \code{\link{runWorkflow}}
}
\examples{
showClass("Workflow")

w1 <- Workflow("mySolution",par1="gaussian",k=10)
w1

w2 <- Workflow("xpto",nmodels=400,ws=1/10,wfID="ensemble420")
w2

}
\keyword{classes}
