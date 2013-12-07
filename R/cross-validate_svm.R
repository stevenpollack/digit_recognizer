### check that data is loaded
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}

library(e1071) # for classAgreement()
library(kernlab) # for ksvm()
library(doMC) # parallel backend for plyr
library(plyr) # advanced apply* functions
library(reshape2) # for data-viz
library(ggplot2) # for data-viz

### register parallel backend
registerDoMC(cores=detectCores()) 

train.on.subset <- F # set to F when on cluster.

if (train.on.subset) {
  training.set <- 1:100
} else {
  training.set <- 1:42000
}

cost.seq <- seq(from=1,to=100,length.out=25)
model.seq <- llply(.data=cost.seq,.parallel=T,.fun=function(cost){
  training.time <- system.time(svm.model <- ksvm(label~., data=digit.data[training.set,], C=cost, cross=10))
  list(svm=svm.model, time=training.time)
})

cv.seq <- laply(.data=model.seq, .parallel=T, .fun=function(model){
  svm.model <- model$svm
  matrix(svm.model@cross,nrow=1)
})

best.cost <- cost.seq[which.min(cv.seq)]
best.model <- ksvm(label~., data=digit.data[training.set,], C=best.cost)

workspace.vars <- ls()[which(!ls() %in% c("digit.data","training.set"))]

save(list=workspace.vars, file=paste("cross-validated_svm_results-", format(Sys.time(), "%H:%M:%S-%d-%m-%Y"),".Rdata",sep=""))
