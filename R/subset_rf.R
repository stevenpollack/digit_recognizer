### check that data is loaded
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}

### partition data into 1000 obs learning and test sets
learning.set <- digit.data[1:1000,]
test.set <- digit.data[41001:42000,]

library(randomForest)

### set up Parallel-RNG infrastructure
library(plyr)
library(doMC)
library(rlecuyer) 
registerDoMC(cores=detectCores())

### Breiman suggests looking at m_{try} \in {default, 0.5*default, 2*default}
### extend search to 1/4*default - 4*default
mtry.seq <- round(seq(from=0.25*28,to=4*28,length.out=20))
ntree.seq <- c(250,500,750,1000,1250,1500)
par.grid <- expand.grid(mtry=mtry.seq,ntree=ntree.seq)

# search over parameter grid for most accurate combination
set.seed(1234)
RNGkind(kind="L'Ecuyer-CMRG")
search.time <- system.time(rf.seq <- dlply(.data=par.grid,.variables=.(mtry,ntree),.parallel=T,
      .fun=function(row){
  rf.tmp <- randomForest(label~.,data=learning.set, xtest=test.set[,-1], ytest=test.set[,1],norm.votes=F,ntree=row$ntree,mtry=row$mtry)
  rf.test.accuracy <- sum(diag(rf.tmp$test$confusion[,1:10]))/dim(test.set)[1]
  list(rf=rf.tmp,accuracy=rf.test.accuracy)
  }))
#    user   system  elapsed 
# 1203.846   18.084 1742.269 

### check out accuracy on test and OOB
accuracy.seq <- laply(.data=rf.seq, .fun=function(list.obj) {
  matrix(list.obj$accuracy,nrow=1)
})

oob.seq <- laply(.data=rf.seq, .fun=function(list.obj) {
  matrix(sum(diag(list.obj$rf$confusion[,1:10]))/dim(learning.set)[1],nrow=1)
})

optimal.pars <- par.grid[which(accuracy.seq == max(accuracy.seq)),]
optimal.par <- optimal.pars[which.min(optimal.pars[,2]),]

optimal.pars2 <- par.grid[which(oob.seq == max(oob.seq)),]
optimal.par2 <- optimal.pars2[which.min(optimal.pars2[,2]),]

### report the best estimators wrt OOB and test set accuracy estimates
### first row corresponds to best test.set estimator
### second row correpsonds to best OOB estimator
optimal.pars.df <- if (sum(optimal.par == optimal.par2)==2) {
  data.frame(optimal.par,"oob accuracy"=oob.seq[as.numeric(rownames(optimal.par))], "test set accuracy"=accuracy.seq[as.numeric(rownames(optimal.par))])
} else {
  data.frame(rbind(optimal.par,optimal.par2),"oob accuracy"=rbind(oob.seq[as.numeric(rownames(optimal.par))],oob.seq[as.numeric(rownames(optimal.par2))]), "test set accuracy"=rbind(accuracy.seq[as.numeric(rownames(optimal.par))],accuracy.seq[as.numeric(rownames(optimal.par2))]))
}

### rf.seq follows grid.expand(ntree,mtry) NOT par.grid
### find estimator corresponding to optimal.parX
.findRF <- function(ntree,mtry) {
  rf.indx <- as.matrix(expand.grid(ntree=ntree.seq, mtry=mtry.seq))
  match.ntree <- which( rf.indx[,1] %in% ntree ) 
  match.mtry <- which( rf.indx[,2] %in% mtry )
  obj.indx <- match.ntree[which( match.ntree %in% match.mtry )]
  rf.seq[[obj.indx]]$rf
}

best.test.set.rf <- .findRF(optimal.pars.df[1,]$ntree, optimal.pars.df[1,]$mtry)
best.oob.rf <- .findRF(optimal.pars.df[2,]$ntree, optimal.pars.df[2,]$mtry)

.aggregate.df <- data.frame(best.test.set.rf$votes[1:25,],name=rownames(best.test.set.rf$votes[1:25,]),label=learning.set[1:25,1],predicted=best.test.set.rf$predicted[1:25])
colnames(.aggregate.df)[1:10] <- as.character(0:9)

# circle = true label, square = voted label
label.vote.plot <- ggplot(data=melt(.aggregate.df,id.vars=c("name","label","predicted"))) + geom_point(aes(x=name,y=label,shape="True"),alpha=0.05,color="black",size=7) + geom_point(aes(x=name,y=predicted,shape="Predicted"),alpha=0.05,color="black",size=8) + geom_point(aes(x=name,y=as.factor(variable),alpha=value/250,color=as.factor(variable),group=label:name),size=3)  + theme_bw() + labs(y="Label",x=NULL,title="Label vote proportion for various training examples") + scale_color_discrete(guide=F) + scale_alpha_continuous(guide=F) + scale_shape_manual(values=c(21,22),labels=c("True","Predicted"),name="") + theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(legend.position="bottom")

### visualize grid accuracy
library(reshape2)
library(ggplot2)

plot.title <- substitute(paste("Most accurate estimator (", perc, "%) at ", n[tree] == opt1, ", ", m[try] == opt2, sep=""),list(perc=max(accuracy.seq)*100,opt1=as.numeric(optimal.par[2]),opt2=as.numeric(optimal.par[1])))

par.grid.heatmap <- ggplot(data=melt(accuracy.seq),aes(y=as.factor(mtry),x=as.factor(ntree),fill=value)) + geom_tile() + labs(x=expression(n[tree]),y=expression(m[try]),title=plot.title) + scale_fill_continuous(name="Accuracy")

plot.title2 <- substitute(paste("Most accurate (OOB) estimator (", perc, "%) at ", n[tree] == opt1, ", ", m[try] == opt2, sep=""),list(perc=max(oob.seq)*100,opt1=as.numeric(optimal.par2[2]),opt2=as.numeric(optimal.par2[1])))

par.grid.heatmap2 <- ggplot(data=melt(oob.seq),aes(y=as.factor(mtry),x=as.factor(ntree),fill=value)) + geom_tile() + labs(x=expression(n[tree]),y=expression(m[try]),title=plot.title2) + scale_fill_continuous(name="Accuracy")

accuracy.difference.heatmap <- ggplot(data=melt(oob.seq-accuracy.seq),aes(y=as.factor(mtry),x=as.factor(ntree),fill=value)) + geom_tile() + labs(x=expression(n[tree]),y=expression(m[try]),title="OOB - Test set accuracy") + scale_fill_continuous(name="Difference")


workspace.vars <- ls()[which(!ls() %in% c("digit.data","learning.set","test.set"))]
save(list=workspace.vars, file=paste("subset_rf-results-", format(Sys.time(), "%H:%M:%S-%d-%m-%Y"),".Rdata",sep=""))