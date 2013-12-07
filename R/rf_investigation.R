### check that data is loaded
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}

### partition data into learning and test sets
sample.sizes <- c(200*2^(0:5),10000,200*2^6,15000,20000,28000,35000,40000)
test.set <- digit.data[40001:42000,]

library(randomForest)

### set up Parallel-RNG infrastructure
library(plyr)
library(doMC)
library(rlecuyer) 
registerDoMC(cores=detectCores())

### Breiman suggests looking at m_{try} \in {default, 0.5*default, 2*default}
### so keep ntree to default but search without mtry
mtry.seq <- c(14,28,56)
par.grid <- expand.grid(training.size=sample.sizes[1:7], mtry=mtry.seq)

RNGkind(kind="L'Ecuyer-CMRG")
rf.seq <- dlply(.data=par.grid,.variables=.(mtry,training.size),.parallel=T,.fun=function(row){
  train.time <- system.time(rf.tmp <- randomForest(label~.,data=digit.data[1:row$training.size,], xtest=test.set[,-1], ytest=test.set[,1],norm.votes=F,mtry=row$mtry))
  rf.test.accuracy <- sum(diag(rf.tmp$test$confusion[,1:10]))/dim(test.set)[1]
  list(rf=rf.tmp,accuracy=rf.test.accuracy,train.time=train.time)
}))

# save intermediate work
workspace.vars <- ls()[which(!ls() %in% c("digit.data","learning.set","test.set"))]
file.name <- paste("rf_investigation-results-", format(Sys.time(), "%H:%M:%S-%d-%m-%Y"),".Rdata",sep="")
save(list=workspace.vars, file=file.name)

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

# save remainder of workspace
workspace.vars <- ls()[which(!ls() %in% c("digit.data","learning.set","test.set"))]
save(list=workspace.vars, file=file.name))