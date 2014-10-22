################################################################# 
## THIS FILE CONTAINS THE METHODS OF THE CLASSES THAT WERE      #
## DEFINED IN THIS PACKAGE                                      #
#################################################################
## Author : Luis Torgo (ltorgo@dcc.fc.up.pt)     Date: Nov 2013 #
## License: GPL (>= 2)                                          #
#################################################################




################################################################
## PredTask methods
################################################################


setMethod("show",
          "PredTask",
          function(object) {
            cat('Prediction Task Object:\n')
            cat('\tTask Name                    ::',object@taskName,"\n")
            cat('\tTask Type                    ::',
                if (object@type == "class") "classification" else "regression","\n")
            cat('\tTarget Feature               ::',object@target,"\n")
            cat('\tFormula                      :: ')
            print(object@formula)
            cat('\tTask Data Source Object Name ::',object@dataSource,"\n")
          }
          )




################################################################
## Workflow methods
################################################################

setMethod("show",
          "Workflow",
          function(object) {
            cat('Workflow Object:\n\tWorkflow ID       :: ',object@name,
                                '\n\tWorkflow Function :: ',object@func)
            if (length(object@pars)) {
                cat('\n\t\tParameter values:\n')
                for(n in names(object@pars)) {
##                    cat('\t\t',n,' = ',deparse(object@pars[[n]]),'\n')
                    k <- object@pars[[n]]
                    k <- if (is.list(k)) paste(paste(names(k),k,sep='='),collapse=" ") else paste(k,collapse=' ')
                    k <- if (nchar(k) > 20) paste(substr(k,1,20),' ...') else k
                    cat('\t\t',n,' -> ',k,'\n')
                }
            }
            cat('\n')
          }
          )


setMethod("summary",
          "Workflow",
          function(object) {
              print(object)
              cat("\nTo apply the workflow on some predictive task use the function 'runWorkflow()'.",
                  "\nExample: 'runWorkflow(myWFobject,Y ~ .,trainData,testData)'\n")
          }
          )



################################################################
## WFoutput methods
################################################################

setMethod("show",
          "WFoutput",
          function(object) {
            cat('WFoutput object :\n')
            cat("\t'predictions' data frame slot with dimensions ",nrow(object@predictions),"x",ncol(object@predictions),"\n")
            if (length(object@extraInfo)) 
              cat("\tObject contains extra information returned by the used workflow function.\n")
            cat("\n")
          })


#setMethod("summary",
#          "WFoutput",
#          function(object) {
#            cat('Summary of workflow output object\n')
#            cat("Scores:\n ")
#            print(object@scores)
#            cat("\n")
#            if (length(object@predictions))
#              cat("Object contains predicted and true values of the target variable. \n\tUse# 'workflowPredictions(obj)' to inspect them.\n")
#            if (length(object@extraInfo)) 
#              cat("Object contains extra information returned by the used workflow function.# \n\tUse 'workflowInformation(obj)' to inspect it.\n")
#            cat("\n")
#          })




################################################################
## CvTask Methods:
################################################################
setMethod("show",
          "CvTask",
          function(object) {
            userSplit <- !is.null(object@dataSplits)
            cat(ifelse(!userSplit & object@strat,'Stratified ',''),
                object@nReps,'x',object@nFolds,
                '- Fold Cross Validation for estimating ',
                paste(object@metrics,collapse=","),"\n")
            if (!userSplit)
              cat('\t Run with seed = ', object@seed,'\n')
            else
              cat('\t User-supplied data splits\n')
          })







################################################################
## HldTask methods:
################################################################

setMethod("show",
          "HldTask",
          function(object) {
            userSplit <- !is.null(object@dataSplits)
            cat(ifelse(!userSplit & object@strat,'\n Stratified ','\n'),
                object@nReps,'x',
                100*(1-object@hldSz),'%/',100*object@hldSz,'% Holdout for estimating ',
                paste(object@metrics,collapse=","),"\n")
            if (!userSplit)
              cat('\t Run with seed = ',object@seed,'\n')
            else
              cat('\t User-supplied data splits\n')
          })




################################################################
## LoocvTask methods:
################################################################


setMethod("show","LoocvTask",
          function(object) {
              userSplit <- !is.null(object@dataSplits)
              cat('\n LOOCV experiment for estimating ',
                paste(object@metrics,collapse=","),"\n")
              if (!userSplit)
                  cat('\t Run with verbose = ',ifelse(object@verbose,'TRUE','FALSE'),' and seed = ',object@seed,'\n')
              else
                  cat('\t Run with verbose = ',ifelse(object@verbose,'TRUE','FALSE'),' and user-supplied data splits\n')
         })





################################################################
## BootTask methods:
################################################################


setMethod("show",
          "BootTask",
          function(object) {
            userSplit <- !is.null(object@dataSplits)
            cat('\n',object@nReps,' repetitions of ',ifelse(object@type=='e0','e0','.632'),
                ' Bootstrap experiment for estimating ',
                paste(object@metrics,collapse=","),'\n')
            if (!userSplit)
              cat('\t Run with seed = ', object@seed,'\n')
            else
              cat('\t User-supplied data splits\n')
         })




################################################################
## McTask methods:
################################################################


setMethod("show",
          "McTask",
          function(object) {
            userSplit <- !is.null(object@dataSplits)
            cat('\n',object@nReps,
               ' repetitions Monte Carlo Simulation for estimating ',
                paste(object@metrics,collapse=","))
            if (userSplit) {
              cat(' using user-supplied data splits\n')
            } else {
              cat(' using:',
                  '\n\t seed = ', object@seed,
                  '\n\t train size = ',object@szTrain,
                  ifelse(object@szTrain<1,'x NROW(DataSet)',' cases'),
                  '\n\t test size = ',object@szTest,
                  ifelse(object@szTest<1,'x NROW(DataSet)',' cases'),
                  '\n'
                  )
            }
         })





################################################################
## EstimationResults methods:
################################################################

setMethod("show",
          "EstimationResults",
          function(object) {
            print(object@estTask)
            cat("\nTask    :: ",object@task@taskName,"\nWorflow :: ",object@workflow@name,"\n")
            cat("\nOverview of the Scores of the experiment:\n")
            print(.scores2summary(object)[1:2,,drop=FALSE])
            if (any(sapply(object@iterationsPreds,length))) 
                cat("\nTrue and Predicted values for each test set are available.\n")
            if (any(sapply(object@iterationsInfo,length))) 
                cat("\nExtra Information returned from the workflow iterations available.\n")
            cat("\n")
            })



setMethod("summary",
          "EstimationResults",
          function(object) {
              cat('\n*** Summary of a ',
                  switch(class(object@estTask),
                         CvTask='Cross Validation',
                         HldTask='Hold Out',
                         McTask='Monte Carlo',
                         BootTask='Bootstrap',
                         LoocvTask='Loocv',
                         ),
                  ' Estimation Experiment ***\n')

              print(object@estTask)
              cat('\n* Predictive Task :: ',object@task@name)
              cat('\n* Workflow        :: ',object@workflow@func,' with parameters ')
              for(x in names(object@workflow@pars)) {
                  k <- object@workflow@pars[[x]]
                  k <- if (is.list(k)) paste(paste(names(k),k,sep='='),collapse=" ") else paste(k,collapse=' ')
                  k <- if (nchar(k) > 20) paste(substr(k,1,20),' ...') else k
                  cat('\n\t',x,' -> ',k,' ')
              }
              cat('\n\n* Summary of Score Estimation Results:\n\n')
              print(.scores2summary(object))
          })


if (!isGeneric("plot"))  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

setMethod("plot",
          "EstimationResults",
          function(x,y,...) {
              
              nstats <- ncol(x@iterationsScores)
              
              tit <- paste(x@workflow@name,
                  switch(class(x@estTask),
                         CvTask='Cross Validation',
                         HldTask='Hold Out',
                         LoocvTask='Leave One Out',
                         BootTask='Bootstrap',
                         McTask='Monte Carlo'
                         ),
                           "estimation on",x@task@name,sep=" "
                  )
              if (nstats == 1) {
                  plt <- qplot(1:nrow(x@iterationsScores),
                               x@iterationsScores[,1],
                               main=tit,
                               xlab='Estimation Iterations',
                               ylab=colnames(x@iterationsScores)[1]) +
                         geom_smooth(method='loess',size=1) +
                         scale_x_discrete()
                  ##print(plt)
              } else {
                  dt <- .scores2long(x@iterationsScores)
                  plt <- ggplot(dt,aes_string(x="rep",y="score")) + 
                      ggtitle(tit) +
                          ylab('Metrics Scores') + xlab('Estimation Iterations')+
                              geom_smooth(aes_string(group="stat"),method='loess',size=1) +
                                  scale_x_discrete() +theme(axis.text.x=element_text(angle=270,size=10,vjust=0.5,hjust=0))+
                                      facet_grid( stat ~ .,scales = "free_y")
                  ##print(plt)
              }
              plt
              
          }
          )



################################################################
# Comparison Results methods
################################################################

setMethod("plot",
          "ComparisonResults",
          function(x,y,...) {
              
              allRes <- NULL
              taskNames <- names(x@tasks)
              for(t in 1:length(x@tasks)) {
                  task <- taskNames[t]
                  sysNames <- names(x@tasks[[t]])
                  for(s in 1:length(x@tasks[[t]])) {
                      d <- .scores2long(x@tasks[[t]][[s]]@iterationsScores)
                      d <- cbind(d,sys=sysNames[s],task=taskNames[t])
                      allRes <- rbind(allRes,d)
                  }
              }

              tlt <- paste(switch(class(x@tasks[[1]][[1]]@estTask),
                                  CvTask='Cross Validation',
                                  HldTask='Hold Out',
                                  LoocvTask='Leave One Out',
                                  BootTask='Bootstrap',
                                  McTask='Monte Carlo'
                                  ),"Performance Estimation Results")
              plt <- ggplot(allRes,aes_string(y="score",x="sys")) +
                     geom_boxplot(aes_string(group="sys")) + ggtitle(tlt) +
                     ylab("Distribution of Statistics Scores") + xlab("Alternative Workflows") +
                         facet_grid(stat ~ task,scales="free_y")+theme(axis.text.x=element_text(angle=270,size=10,vjust=0.5))
              #print(plt)
              plt
                      
  })



setMethod("summary",
          "ComparisonResults",
          function(object) {
              cat('\n== Summary of a ',
                  switch(class(object@tasks[[1]][[1]]@estTask),
                         CvTask='Cross Validation',
                         HldTask='Hold Out',
                         LoocvTask='Leave One Out',
                         BootTask='Bootstrap',
                         McTask='Monte Carlo'
                         ),
                  'Performance Estimation Experiment ==\n')
              print(object@tasks[[1]][[1]]@estTask)
              cat('\n* Predictive Tasks :: ',
                  paste(names(object@tasks),collapse=', '))
              cat('\n* Workflows  :: ',paste(names(object@tasks[[1]]),collapse=', '),"\n")
              
              ##cat('\n\n* Summary of Experiment Results:\n')
              ld <- list()
              for(d in 1:length(object@tasks)) {
                  lv <- list()
                  cat("\n-> Task: ",names(object@tasks)[d])
                  for(v in 1:length(object@tasks[[d]])) {
                      cat("\n  *Workflow:",names(object@tasks[[d]])[v],"\n")
                      ss <- .scores2summary(object@tasks[[d]][[v]])
                      print(ss)
                      lv <- c(lv,list(ss))
                  }
                  ##cat('\n')
                  names(lv) <- names(object@tasks[[d]])
                  ld <- c(ld,list(lv))
              }
              names(ld) <- names(object@tasks)
              invisible(ld)
              
          })



setMethod("show",
          "ComparisonResults",
          function(object) {
            cat('\n== ',
                switch(class(object@tasks[[1]][[1]]@estTask),
                       CvTask='Cross Validation',
                       HldTask='Hold Out',
                       BootTask='Bootstrap',
                       LoocvTask='Leave One Out',
                       McTask='Monte Carlo'
                       ),
                'Performance Estimation Experiment ==\n\n')
            print(object@tasks[[1]][[1]]@estTask)
            cat(length(object@tasks[[1]]),' workflows\n')
            cat('tested on ',length(object@tasks),' predictive tasks\n')
          })



# =====================================================
# Method that selects a subset of the experimental 
# results in a object.
# The subsetting criteria can be one of the four dimensions
# of the foldResults array, i.e. the iterations, the statistcs,
# the workflow variants, and the data sets, respectively.
# Subsetting expressions can be provided as numbers or as
# dimension names.
# =====================================================
# Luis Torgo, Aug 2009
# =====================================================
# Example runs:
# > plot(subset(nnet,stats='e1',vars=1:4))
#
setMethod("subset",
          signature(x='ComparisonResults'),
          function(x,
                 tasks=1:length(x@tasks),
                 workflows=1:length(x@tasks[[1]]),
                 statistics=1:dim(x@tasks[[1]][[1]]@iterationsScores)[2])
          {
            rr <- x
            if (!identical(workflows,1:length(x@tasks[[1]]))) {
              if (is.character(workflows) && length(workflows) == 1)
                workflows <- grep(workflows,names(rr@tasks[[1]]))
              rr@tasks <- lapply(rr@tasks,function(t) t[workflows])
            }
            if (!identical(tasks,1:length(x@tasks))) {
              if (is.character(tasks) && length(tasks) == 1)
                tasks <- grep(tasks,names(rr@tasks))
              rr@tasks <- rr@tasks[tasks]
            }
            if (is.character(statistics) && length(statistics) == 1) 
                statistics <- grep(statistics,colnames(x@tasks[[1]][[1]]@iterationsScores))
            rr@tasks <- lapply(rr@tasks,function(t) lapply(t,function(s) {sn <- s; sn@iterationsScores <- s@iterationsScores[,statistics,drop=F] ; sn}))

            rr
          }
          )





