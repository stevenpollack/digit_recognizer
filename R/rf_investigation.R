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
ntree.seq <- c(100,500,1000,1500)
par.grid <- expand.grid(training.size=sample.sizes, mtry=mtry.seq, ntree=ntree.seq)

RNGkind(kind="L'Ecuyer-CMRG")
rf.seq <- dlply(.data=par.grid,.variables=.(mtry,training.size,ntree),.parallel=T,.fun=function(row){
  train.time <- system.time(rf.tmp <- randomForest(label~.,data=digit.data[1:row$training.size,], xtest=test.set[,-1], ytest=test.set[,1],norm.votes=F,mtry=row$mtry,ntree=row$ntree))
  rf.test.accuracy <- sum(diag(rf.tmp$test$confusion[,1:10]))/dim(test.set)[1]
  list(rf=rf.tmp,accuracy=rf.test.accuracy,train.time=train.time)
})

# save intermediate work
workspace.vars <- ls()[which(!ls() %in% c("digit.data","learning.set","test.set"))]
file.name <- paste("rf_investigation-results-", format(Sys.time(), "%H:%M:%S-%d-%m-%Y"),".Rdata",sep="")
save(list=workspace.vars, file=file.name)

### checkout accuracy and timing
accuracy.seq <- laply(.data=rf.seq, .fun=function(list.obj) {
  matrix(list.obj$accuracy,nrow=1)
})

timing.seq <- laply(.data=rf.seq, .fun=function(list.obj) {
  matrix(list.obj$train.time[3],nrow=1)
})

par.grid.results <- cbind(par.grid,accuracy=as.vector(t(accuracy.seq)),timing=as.vector(t(timing.seq)))

### visualize grid
library(reshape2)
library(ggplot2)

par.grid.heatmap <- ggplot(data=melt(par.grid.results,id.vars=c("training.size","mtry")),aes(y=value,x=as.factor(training.size),color=as.factor(mtry),group=mtry )) + geom_point() + geom_line() + facet_grid(variable~.,scales="free_y") + labs(x="training set size",y="",title=expression(paste("Accuracy and training timing for Random Forest when ", n[tree] == 500, sep=""))) + scale_color_discrete(name=expression(m[try]))

# show(par.grid.heatmap)

# rf.seq2 <- llply(.data=c(100,1000,1500),.parallel=T,.fun=function(ntree){
#   train.time <- system.time(rf.tmp <- randomForest(label~.,data=digit.data[1:10000,], xtest=test.set[,-1], ytest=test.set[,1],norm.votes=F,mtry=14,ntree=ntree))
#   rf.test.accuracy <- sum(diag(rf.tmp$test$confusion[,1:10]))/dim(test.set)[1]
#   list(rf=rf.tmp,accuracy=rf.test.accuracy,train.time=train.time)
# })

# save remainder of workspace
workspace.vars <- ls()[which(!ls() %in% c("digit.data","learning.set","test.set"))]
save(list=workspace.vars, file=file.name)