\name{classificationMetrics}
\alias{classificationMetrics}

\title{
Calculate some standard classification evaluation metrics of predictive performance
}
\description{
This function is able to calculate a series of classification evaluation
statistics given two vectors: one with the true target variable values,
and the other with the predicted target variable values. Some of the
metrics may require additional information to be given (see Details section).		

}
\usage{
classificationMetrics(trues,preds,
            stats="err",
            benMtrx=NULL,
            allCls=unique(c(levels(as.factor(trues)),levels(as.factor(preds)))),
            posClass=allCls[1],
            beta=1)
}

\arguments{
  \item{trues}{
A vector or factor with the true values of the target variable.
}
  \item{preds}{
A  vector or factor with the predicted values of the target variable.
}
  \item{stats}{
    A vector with the names of the evaluation statistics to
    calculate (see Details section). If none is indicated it will
  default to the error rate ("err"). It is also possible to
  indicate the value "all", and all available metrics will be calculated.
  }
  \item{benMtrx}{
    An optional cost/benefit matrix with numeric values representing the benefits (positive
    values) and costs (negative values) for all combinations of predicted
    and true values of the nominal target variable of the task. In this
    context, the matrix should have the dimensions C x C, where C is
    the number of possible class values of the classification task. Benefits
    (positive values) should be on the diagonal of the matrix (situations
    where the true and predicted values are equal, i.e. the model
    predicted the correct class and thus should be rewarded for that),
    whilst costs (negative values) should be on all positions outside of
    the diagonal of the matrix (situations where the predicted value is
    different from the true class value and thus the model should incur on
    a cost for this wrong prediction).
  }
  \item{allCls}{
    An optional vector with the possible values of the nominal target variable,
    i.e. a vector with the classes of the problem. The default of this
    parameter is to infer these values from the given vector of true and
  predicted values
    values. However, if these are small vectors (e.g. you are evaluating
    your model on a small test set), it may happen that not all possible class
    values occur in this vector and this will potentially create problems
    in the sub-sequent calculations. Moreover, even if the vector is not
    small, for highly unbalanced classification tasks, this problem may
    still occur. In these contexts, it is safer to specifically indicate
    the possible class values through this parameter.
  }
    \item{posClass}{
    An optional string with the name of the class (a value of the target
  nominal variable) that should be considered the "positive" class. This
  is used typically on two class problems where one of the classes is
  more relevant than the other (the positive class). It will default to
  the first value of the vector of possible classes (\code{allCls}).
  }
    \item{beta}{
    An optional number for the value of the Beta parameter in the
  calulation of the F-measure (defaults to 1 that corresponds to giving
  equal relevance to precision and recall in the calculation of the F score).
  }

}
\details{
In the
  following description of the currently available metrics we denote the
  vector of true target variable values as t, the vector of predictions
  by p, while N denotes the size of these two vectors, i.e. the number
  of test cases. Furthermore we will denote the number of classes
  (different values of the target nominal variable) as C. For problems
  with only two classes, where one is considered the "positive" class we
  will use some extra notation, namely: TP = #{p == + & t == +} ; FP =
  #{p == + & t == -} ; TN = #{p == - & t == -}; FN = #{p == - & t == +}
  ; P = #{t == +} ; N = #{t == -}

  The currently available classification performance metrics
  are:
  
  "acc": sum(I(t_i == p_i))/N, where I() is an indicator function given 1 if its
  argument is true and 0 otherwise. Note that "acc" is a value in the interval [0,1], 1
  corresponding to  all predictions being correct.

  "err": the error rate, calculated as 1 - "acc"

  "totU": this is a metric that takes into consideration not only
  the fact that the predictions are correct or not, but also the costs or
  benefits of these predictions. As mentioned above it assumes that the
  user provides a fully specified cost/benefit matrix though parameter \code{benMtrx}, with
  benefits corresponding to correct predictions, i.e. where t_i ==
  p_i, while costs correspond to erroneous predictions. These matrices are C x C square matrices, where C is the
  number of possible values of the nominal target variable (i.e. the
  number of classes). The diagonal of these matrices corresponds to the
  correct predictions (t_i == p_i) and should have positive values
  (benefits). The positions outside of the diagonal correspond to
  prediction errors and should have negative values (costs). The "totU"
  measures the total Utility (sum of the costs and benefits) of the
  predictions of a classification model. It is calculated as
  sum(CB[t_i,p_i] * CM[t_i,p_i) where CB is a cost/benefit matrix and CB[t_i,p_i] 
  is the entry on this matrix corresponding to predicting class p_i for
  a true value of t_i, and CM is a confusion matrix where CM[t_i,p_i] is
  the number of times the true class value was t_i and the prediction of
  the model was p_i

  "fpr": false positives rate, is a metric applicable to two classes
  tasks that measures the proportion of times the model
  forecasted a positive class when it should not and it is given by FP/P

  "fnr": false negatives rate, is a metric applicable to two classes
  tasks that measures the proportion of times the model
  forecasted a negative class when it should not, and it is given by FN/P

  "tpr": true positives rate, is a metric applicable to two classes
  tasks that measures the proportion of times the model
  forecasted a positive class for the positive test cases, and it is given by TP/P

  "tnr": true negatives rate, is a metric applicable to two classes
  tasks that measures the proportion of times the model
  forecasted a negative class for the negative test cases, and it is
  given by TN/N

  "rec": recall, it is equal to the true positive rate ("tpr")

  "sens": sensitivity, it is equal to the true positive rate ("tpr")

  "spec": specificity, it is a metric  applicable to two classes
  tasks that measures the proportion of times the model
  forecasted a negative class for the negative test cases, and it is
  given by TN/N

  "prec": precision, it is a metric  applicable to two classes
  tasks that measures the proportion of times the model
  forecasted a positive class and it was correct, and it is
  given by TP/(TP+FP)

  "rpp": rate of positive predictions, it is a metric  applicable to two classes
  tasks that measures the proportion of times the model
  forecasted a positive class, and it is
  given by (TP+FP)/N

  "lift": lift, it is a metric  applicable to two classes
  tasks and it is given by TP/P/(TP+FP) or equivalently TP/(P*TP+P*FP)

  "F": the F-nmeasure, it is a metric  applicable to two classes
  tasks that considers both the values of precision and recall weighed
  by a parameter Beta (defaults to 1 corresponding to equal weights to both), and it is
  given by (1+Beta^2)*("prec" * "rec") / ( (Beta^2 * "prec") + "rec")

  "microF": micro-averaged F-measure, it is equal to accuracy ("acc")

  "macroF": macro-averaged F-measure, it is the average of the F-measure
  scores calculated by making the positive class each of the possible
  class values in turn

  "macroRec": macro-averaged recall, it is the average recall by making
  the positive class each of the possible  class values in turn  

  "macroPrec": macro-averaged precision, it is the average precision by making
  the positive class each of the possible  class values in turn  
}
\value{
A named vector with the calculated statistics.
}
\references{ Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).

  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR}  
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\note{
  1. In case you require "totU" to be calculated you must supply a
  cost/benefit matrix through parameter \code{benMtrx}.

  2. If not all possible class values are present in the vector of true
  values  in parameter \code{trues}, you should provide a vector
  with all the possible class values in parameter \code{allCls}.
}
\seealso{
 \code{\link{regressionMetrics}}, \code{\link{timeseriesMetrics}}
}
\examples{
\dontrun{
library(DMwR)
## Calculating several statistics of a classification tree on the Iris data
data(iris)
idx <- sample(1:nrow(iris),100)
train <- iris[idx,]
test <- iris[-idx,]
tree <- rpartXse(Species ~ .,train)
preds <- predict(tree,test,type='class')
## Calculate the  error rate
classificationMetrics(test$Species,preds)
## Calculate the all possible error metrics
classificationMetrics(test$Species,preds,stats="all")
## Now trying calculating the utility of the predictions
cbM <- matrix(c(10,-20,-20,-20,20,-10,-20,-10,20),3,3)
classificationMetrics(test$Species,preds,"totU",cbM)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
