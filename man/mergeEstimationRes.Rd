\name{mergeEstimationRes}
\alias{mergeEstimationRes}
\title{
Merging several \code{\linkS4class{ComparisonResults}} class objects
}
\description{
  This function can be used to join several objects of class \code{ComparisonResults}  
  into a single object. The merge is carried out assuming that there is
  something in common between the objects (e.g. all use the same
  workflows on different tasks), and that the user specifies which
  property should be used for the merging process.
}
\usage{
mergeEstimationRes(..., by = "tasks")
}

\arguments{
  \item{\dots}{
The \code{\linkS4class{ComparisonResults}} class object names separated by commas
}
\item{by}{
  The dimension of the \code{\linkS4class{ComparisonResults}} class objects that should be used
  for the merge. All objects should have the same values on the
  remaining dimensions of an estimation experiment. For instance, if you
  merge by "tasks" (the default) it means that the objects being merged
  should include estimation results on the same set of workflows on the
  same set of metrics, using the same estimation method and
  settings. The only thing that changes between the objects in this
  example is the set of tasks. Possible values of this argument are:
  "tasks", "workflows" and "metrics".
}
}
\details{
  The objects of class \code{\linkS4class{ComparisonResults}} (type "class?ComparisonResults"
    for details) contain several information on the results of an
  estimation expriment for several workflows on several predictive
  tasks. Sometimes, when you are trying too many workflows on too many
  tasks, it is convinient to run these variants on different calls to
  the function \code{\link{performanceEstimation}}. After all calls are
  completed we frequently want to have all results on a single
  object. This is the objective of the current function: allow you to
  merge these different \code{\linkS4class{ComparisonResults}} objects
  into a single one. For being mergeable the objects need to have things
  in common otherwise it makes no sense to merge them. For instance, we
  could split our very large experiment by calling
  \code{\link{performanceEstimation}} with different tasks, although the
  rest (the workflows and the estimation task) stays the same. See the
  Examples section for some illustrations.

}
\value{
  The result of this function is a \code{ComparisonResults} object.
}
\references{ Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
\code{\link{performanceEstimation}}, \code{\linkS4class{ComparisonResults}}, \code{\link[=subset,ComparisonResults-method]{subset}}
}
\examples{
\dontrun{
## Run some experiments with the swiss data and two different
## prediction models
data(swiss)

exp1 <- performanceEstimation(
  PredTask(Infant.Mortality ~ .,swiss),
  workflowVariants(learner="svm",
                   learner.pars=list(cost=c(1,10),gamma=c(0.01,0.5))),
  EstimationTask("mse")
  )

exp2 <- performanceEstimation(
  PredTask(Infant.Mortality ~ .,swiss),
  Workflow(learner="lm"),
  EstimationTask("mse")
  )


## joining the two experiments by workflows
all <- mergeEstimationRes(exp1,exp2,by="workflows")
topPerformers(all) # check the best results

## now an example by adding new metrics
exp3 <- performanceEstimation(
  PredTask(Infant.Mortality ~ .,swiss),
  Workflow(learner="lm"),
  EstimationTask(metrics=c("mae","totTime"))
  )

allLM <- mergeEstimationRes(exp2,exp3,by="metrics")
topPerformers(allLM) 

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}

