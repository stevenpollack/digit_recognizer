library(data.table)
library(ggplot2)
library(reshape2)

load("pars_nnet_caret_results.Rdata")
caret.results2 <- data.table(melt(caret.results[,-3,with=F],measure.vars=c("Accuracy","Kappa")))[variable=="Accuracy",limit:=AccuracySD*1.96/sqrt(3)][variable=="Kappa",limit:=KappaSD*1.96/sqrt(3)]

pars_nn_metric.vs.size <- ggplot(data=caret.results2,aes(group=as.factor(decay):variable)) + geom_line(aes(x=size,y=value,color=as.factor(decay))) + geom_point(aes(x=size,y=value,color=as.factor(decay))) + facet_grid(variable~.) + geom_errorbar(aes(x=size,ymax=value+limit,ymin=value-limit,color=as.factor(decay)),position="dodge") + labs(x="Size of hidden layer (nodes)",y=NULL) + theme(legend.position="bottom") + scale_color_discrete(name="Decay") + scale_x_continuous(breaks=unique(caret.results2[,size]),labels=levels(as.factor(caret.results2[,size])))

save(caret.results2,pars_nn_metric.vs.size,file="pars_nn-plot_and_df.Rdata")

