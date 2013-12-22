\name{mergeEstimationRes}
\alias{mergeEstimationRes}
\title{
Merging several \code{compExp} class objects
}
\description{
  This function can be used to join several \code{compExp} class objects
  into a single object. The merge is carried out assuming there there is
  something in common between the objects (e.g. all use the same
  learners on different data sets), and that the user specifies which
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
  for the merge. All objects should have the same values on this dimension.
}
}
\details{
  The objects of class \code{\linkS4class{ComparisonResults}} (type "class?ComparisonResults"
    for details) contain several information on the results of an experimental comparison
  between several prediction models on several data sets. These
  experiments are carried out with the function
  \code{performanceEstimation()}. One of the "slots" of the objects of
  class \code{ComparisonResults} contains the actual results of the experiment on
  the different repetitions that were carried out. This slot is an array
  with four dimensions: "iterations","statistics","variants","datasets",
  in this order. This function allows the user the merge several objects
  of this class according to one of these four dimensions. Example uses
  of this function is a user that carries out a similar experiment
  (i.e. with the same experimental settings) on the same data sets
  twice, each time with a different set of learners being compared. This
  user might be interested in mergind the two \code{ComparisonResults} objects
  resulting from these experiments into a single object for comparing
  the results across all learners. This use should then use this
  function to join the two objects by "variants". Another example would
  be a set up where the same experiment with a set of learners was
  repeated with different sets of data sets. All the resulting objects
  vould be merged by "datasets" to obtain a single results object.

  You should note that the merging is only possible if all objects share
  the same experimental settings. Obviously, it only makes sense to
  merge several objects into a single one by some dimension
  "x" if all other dimensions are equal.
}
\value{
  The result of this function is a \code{ComparisonResults} object.
}
\references{ Torgo, L. (2013) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models}.
  \url{https://github.com/ltorgo/performanceEstimation}  
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
  workflowVariants("standardWF",learner="svm",
                   learner.pars=list(cost=c(1,10),gamma=c(0.01,0.5))),
  CvSettings(nReps=1,nFolds=10,seed=1234)
  )

exp2 <- performanceEstimation(
  PredTask(Infant.Mortality ~ .,swiss),
  workflowVariants("standardWF",learner="lm"),
  CvSettings(nReps=1,nFolds=10,seed=1234)
  )


## joining the two experiments by variants (i.e. models)
all <- mergeEstimationRes(exp1,exp2,by="workflows")
topPerformers(all) # check the best results

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}

