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
            cat('\tTask Name :: ',object@name)
            cat('\n\tFormula   :: ')
            print(object@formula)
            cat('\tTask Data ::\n')
            str(object@data,give.attr=F)
            cat('\n')
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
            cat("\tScores on ",length(object@scores),
                " evaluation metrics\n")
            if (length(object@predictions))
              cat("\tObject contains predicted and true values of the target variable\n")
            if (length(object@extraInfo)) 
              cat("\tObject contains extra information returned by the used workflow function.\n")
            cat("\n")
          })


setMethod("summary",
          "WFoutput",
          function(object) {
            cat('Summary of workflow output object\n')
            cat("Scores:\n ")
            print(object@scores)
            cat("\n")
            if (length(object@predictions))
              cat("Object contains predicted and true values of the target variable. \n\tUse 'workflowPredictions(obj)' to inspect them.\n")
            if (length(object@extraInfo)) 
              cat("Object contains extra information returned by the used workflow function. \n\tUse 'workflowInformation(obj)' to inspect it.\n")
            cat("\n")
          })




################################################################
## CvSettings Methods:
################################################################
setMethod("show",
          "CvSettings",
          function(object) {
            userSplit <- !is.null(object@dataSplits)
            cat(ifelse(!userSplit & object@strat,'Stratified ',''),
                object@nReps,'x',object@nFolds,
                '- Fold Cross Validation')
            if (!userSplit)
              cat(' run with seed = ', object@seed,'\n')
            else
              cat('\n   User-supplied data splits\n')
          })







################################################################
## HldSettings methods:
################################################################

setMethod("show",
          "HldSettings",
          function(object) {
            userSplit <- !is.null(object@dataSplits)
            cat(ifelse(!userSplit & object@strat,'\n Stratified ','\n'),
                object@nReps,'x',
                100*(1-object@hldSz),'%/',100*object@hldSz,'% Holdout')
            if (!userSplit)
              cat(' run with seed = ',object@seed,'\n')
            else
              cat('\n   User-supplied data splits\n')
          })




################################################################
## LoocvSettings methods:
################################################################


setMethod("show","LoocvSettings",
          function(object) {
           cat('\n LOOCV experiment with verbose = ',
               ifelse(object@verbose,'TRUE','FALSE'),' and seed =',
               object@seed,'\n')
         })





################################################################
## BootSettings methods:
################################################################


setMethod("show",
          "BootSettings",
          function(object) {
            cat('\n',ifelse(object@type=='e0','e0','.632'),
                ' Bootstrap experiment settings\n\tSeed = ',
                object@seed,'\n\tNr. repetitions = ',object@nReps,'\n')
            if (!is.null(object@dataSplits))
              cat('\n   User-supplied data splits\n')
         })




################################################################
## McSettings methods:
################################################################


setMethod("show",
          "McSettings",
          function(object) {
            userSplit <- !is.null(object@dataSplits)
            cat('\n',object@nReps,
               ' repetitions Monte Carlo Simulation')
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
            print(object@settings)
            cat("\nTask    :: ",object@task@name,"\nWorflow :: ",object@workflow@name,"\n")
            cat("\nOverview of the Scores of the experiment:\n")
            print(.scores2summary(object)[1:2,])
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
                  switch(class(object@settings),
                         CvSettings='Cross Validation',
                         HldSettings='Hold Out',
                         McSettings='Monte Carlo',
                         BootSettings='Bootstrap',
                         LoocvSettings='Loocv',
                         ),
                  ' Estimation Experiment ***\n')

              print(object@settings)
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
                  switch(class(x@settings),
                         CvSettings='Cross Validation',
                         HldSettings='Hold Out',
                         LoocvSettings='Leave One Out',
                         BootSettings='Bootstrap',
                         McSettings='Monte Carlo'
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

              tlt <- paste(switch(class(x@tasks[[1]][[1]]@settings),
                                  CvSettings='Cross Validation',
                                  HldSettings='Hold Out',
                                  LoocvSettings='Leave One Out',
                                  BootSettings='Bootstrap',
                                  McSettings='Monte Carlo'
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
                  switch(class(object@tasks[[1]][[1]]@settings),
                         CvSettings='Cross Validation',
                         HldSettings='Hold Out',
                         LoocvSettings='Leave One Out',
                         BootSettings='Bootstrap',
                         McSettings='Monte Carlo'
                         ),
                  'Performance Estimation Experiment ==\n')
              print(object@tasks[[1]][[1]]@settings)
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
                switch(class(object@tasks[[1]][[1]]@settings),
                       CvSettings='Cross Validation',
                       HldSettings='Hold Out',
                       BootSettings='Bootstrap',
                       LoocvSettings='Leave One Out',
                       McSettings='Monte Carlo'
                       ),
                'Performance Estimation Experiment ==\n\n')
            print(object@tasks[[1]][[1]]@settings)
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





