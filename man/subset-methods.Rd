\name{subset-methods}
\docType{methods}
\alias{subset-methods}
%%\alias{subset,ANY-method}
\alias{subset,ComparisonResults-method}
\title{Methods for Function \code{subset} in Package \pkg{performanceEstimation}}
\description{
  The method subset when applied to objects of class \code{\linkS4class{ComparisonResults}} can
  be used to obtain another object of the same class with a subset of
  the estimation results contained in the original object.
}
\section{Methods}{
\describe{

  \item{\code{signature(x = "ComparisonResults")}}{
    The method has as first argument the object of class \code{\linkS4class{ComparisonResults}}
    that you wish to subset. This method also includes 3 extra arguments that
    you can use to supply the subsetting criteria.

    Namely, the parameter
    \code{metrics} allows you to indicate a vector with the subset of
    evaluation metrics in the orginal object. Alternatively, you can
     provide a regular expression to be matched against the name
    of the statistics measured in the experiment (see function
    \code{\link{metricNames}}) to specify the subset of metrics 
    you want to select.

    The parameter \code{workflows}
    can be used to provide a vector with the subset of workflows
    (approaches to the predictive tasks)
    that are to be used in the sub-setting. Alternatively, you can also
     provide a regular expression to be matched against the name
    of the workflows (see function
    \code{\link{workflowNames}})  evaluated in the experiment to specify the subset
    you want to select.

    The parameter \code{tasks}
    allows you to indicate a vector with the subset of predictive tasks to be
    extracted. Alternatively, you can
     provide a regular expression to be matched against the name
    of the tasks used in the experiment (see function
    \code{\link{taskNames}}) to specify the subset
    you want to select.

    Finally, the parameter \code{partial} allows you to control how the
    names of the other parameters (tasks, workflows and metrics) are
    matched against the original names in the \code{ComparisonResults}
    object. It defaults to \code{TRUE} which means that the function
    \code{\link{grep}} is used, whilst with \code{FALSE} the mathcin is
    done using the function \code{\link{match}}. For instance, if you
    have three metrics being estimated with names "F", "F1" and "F2",
    and you call subset with \code{metrics="F"}, this would in effect
    subset all three metrics, whilst with \code{partial=FALSE} only the
    first of the three would be considered.

}
}}
\keyword{methods}

