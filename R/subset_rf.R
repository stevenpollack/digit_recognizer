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

### find most accurate combo (with smallest ntree, if ties)
accuracy.seq <- laply(.data=rf.seq, .fun=function(list.obj) {
  matrix(list.obj$accuracy,nrow=1)
})

optimal.pars <- par.grid[which(accuracy.seq == max(accuracy.seq)),]
optimal.par <- optimal.pars[which.min(optimal.pars[,2]),]

### visualize grid accuracy
library(reshape2)
library(ggplot2)

plot.title <- substitute(paste("Most accurate estimator when ", n[tree] == opt1, ", ", m[try] == opt2, sep=""),list(opt1=as.numeric(optimal.par[2]),opt2=as.numeric(optimal.par[1])))

par.grid.heatmap <- ggplot(data=melt(accuracy.seq),aes(y=as.factor(mtry),x=as.factor(ntree),fill=value)) + geom_tile() + labs(x=expression(n[tree]),y=expression(m[try]),title=plot.title)


workspace.vars <- ls()[which(!ls() %in% c("digit.data","learning.set","test.set"))]
save(list=workspace.vars, file=paste("subset_rf-results-", format(Sys.time(), "%H:%M:%S-%d-%m-%Y"),".Rdata",sep=""))