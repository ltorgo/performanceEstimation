\name{PredTask-class}
\Rdversion{1.1}
\docType{class}
\alias{PredTask}
\alias{PredTask-class}
\alias{show,PredTask-method}

\title{Class "PredTask" }
\description{	 This is the class of objects that represent a predictive task  }
\section{Objects from the Class}{
  Objects can be created by calls to the constructor
  \code{PredTask(...)}. The constructor requires a formula and a data
  frame on the first two arguments. You may also a name for the task
  through the parameter \code{taskName} of the constructor. Optional
  parameter \code{type} allows you to indicate the type of task (either
  "regr", "class" or "ts", for regression, classification and time
  series tasks, respectively). If not provided this will be inferred
  from constructor. Setting the optional parameter \code{copy} to
  \code{TRUE} (defaults to \code{FALSE})  will force the constructor to make a copy
  of the given data frame and store it in the \code{dataSource} slot of
  the  \code{PredTask} object.
}
\section{Slots}{
	 \describe{
    \item{\code{formula}:}{Object of class \code{formula} containing
      the formula representing the predictive task. }
    \item{\code{dataSource}:}{Object of class \code{data.frame},
      \code{call} or \code{name}. This will be used to fecth the task
      data when necessary. The first of these options will only be used
      if the user calls the constructor with \code{copy=TRUE} and will
      result in the source data being copied into the \code{PredTask} object.}
    \item{\code{taskName}:}{Optional object of class \code{character} containing
      the ID of the predictive task }
    \item{\code{type}:}{Optional object of class \code{character} containing
      the type of the predictive task (it can be "regr", "class" or "ts"). }
    \item{\code{target}:}{Optional object of class \code{character} containing the
     name of the target variable.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "PredTask")}: method used to
      show the contents of a PredTask object. }
	 }
}

\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
}

\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
  \code{\linkS4class{Workflow}},
  \code{\linkS4class{EstimationTask}},
 \code{\link{performanceEstimation}}
}
\examples{
showClass("PredTask")
data(iris)
PredTask(Species  ~ .,iris)
PredTask(Species  ~ .,iris[1:20,],"irisSubset")
## after the next example you can safely remove the iris data frame from
## your environment because the data was stored inside the "t" object.
t <- PredTask(Species ~ ., iris,copy=TRUE) 
}
\keyword{classes}
