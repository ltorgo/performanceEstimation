
require(ggplot2)
require(plyr)
d <- adply(x,c(1,3,4))
colnames(d)[1:3] <- c('Workflow','Metric','Task')
d$signif <- cut(1-d$p.value,c(0,0.7,0.9,0.95,0.99,1))
bestData <- subset(d,Workflow=="svm.v2",c(Task,Metric,AvgScore))

g <- ggplot(data=d,aes(x=Workflow))
g <- g +
    geom_bar(aes(y=AvgScore,fill=signif),stat="identity")+
#    geom_point(aes(y=AvgScore,colour=signif,size=5),stat="identity")+
    ylab("Estimated Average Score") +
    geom_hline(aes(yintercept=AvgScore),data=bestData)+
    coord_flip()+
    facet_grid(Metric ~ Task,scales="free_y")
g

g <- ggplot(data=d,aes(x=Workflow))
g.score <- g +
        geom_bar(aes(y=AvgScore,fill=Workflow))+
        opts(legend.position="none") +
        opts(axis.text.y = theme_blank(), axis.title.y = theme_blank()) + 
        opts(title = 'Estimated Score', plot.title = theme_text( size = 10) ) +  
        coord_flip()



g <- ggplot(data=d,aes(x=Workflow))
g <- g+geom_bar(aes(y=AvgScore,fill=Workflow),stat='identity')
g <- g+geom_bar(aes(y=-p.value,fill=Workflow),stat='identity')+
  scale_y_continuous(labels = comma)

