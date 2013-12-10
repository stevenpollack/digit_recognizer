library(data.table)
<<<<<<< Updated upstream
library(caret)
library(plyr)
library(doMC)
registerDoMC(cores=detectCores())
library(nnet)
=======
>>>>>>> Stashed changes
library(RSNNS)

### load data
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}

estAccuracy <- function(model,newdata, verbose=F,savePreds=F) {
  ### assuming the newdata.input is normalized and properly cleaned
  newdata.input <- newdata[,-1,with=F]
  newdata.output <- newdata[,label]
  pred.time <- system.time(preds <- caret::predict.avNNet(model,newdata.input,type="class"))
  confusionMat <- table(preds,newdata.output)
  accuracy.est <- sum(diag(confusionMat))/length(newdata.output)
  output <- list("Confusion Matrix"=confusionMat, "Accuracy Estimate"=accuracy.est)
  output$predictions <- ifelse(savePreds,preds,NA)
  if (verbose) {
    print(paste("prediction took:", pred.time[3], "seconds"))
    print(output)
  }
  return(output)
}

### 1. find pixels with zero variance (dead)
digit.dt <- data.table(digit.data)
dead.pixels <- which(digit.dt[,!"label",with=F][,lapply(.SD,var)]==0) +1 #+1 due to subsetting

### 2. check that none of these are in rf_selected covariates
load("rf_selected_vars.Rdata")
selected.pixels <- which(colnames(digit.dt) %in% top.40.mdg.and.mda)
selected.pixels <- setdiff(selected.pixels,dead.pixels)

### 3. make routine to build training set
### 3.1 normalize input data
normed.digit.input <- data.table(RSNNS::normalizeData(digit.dt[,-1,with=F]))

build.training.set <- function(sample.size) {
  training.set.x <- normed.digit.input[1:sample.size,selected.pixels-1,with=F]
  training.set.y <- digit.dt[1:sample.size,label]
  data.table(label=training.set.y, training.set.x)
}

### make test set
test.set.x <- normed.digit.input[41001:42000,selected.pixels-1,with=F]
test.set <- data.table(label=digit.data[41001:42000,1], test.set.x)

## prepare kaggle set
load(file="data_and_benchmarks/testing_data.Rdata")
kaggle <- data.table(RSNNS::normalizeData(unlabeled.digit.data[,selected.pixels-1]))

### calculate RoT for size of hidden layer
num.of.inputs <- length(selected.pixels)
num.of.outputs <- 10
num.of.nodes <- mean(c(num.of.inputs,num.of.outputs))

### write parallelized benchmarking
library(doRNG)
library(nnet)
library(caret)
library(doParallel)
registerDoParallel(cores=detectCores())

set.seed(1234)
sample.size.seq <- c(280,480,800,1600,6400,10000,12800,25600,35000,40000)
benchmark.results <- foreach(i=sample.size.seq,.packages='caret',.inorder=FALSE) %dorng% {
  training.set <- build.training.set(i)
  summary(training.set)
  train.time <- system.time( subset.avNNet <- avNNet(formula=label~.,
                                                       data=training.set,
                                                       size=num.of.nodes, decay=0.1, softmax=T,MaxNWts=12000,
                                                       repeats=3,
                                                       bag=F) )
  subset.accuracy.est <- estAccuracy(subset.avNNet,test.set)
  filename <- paste("pars_nn_benchmark_size-",i,".Rdata",sep="")
  subset.out <- list(time=train.time,avNNet=subset.avNNet,accuracy=subset.accuracy.est)
  save(subset.out,file=filename)
  subset.out
}

save(benchmark.results, file=paste("parsimonious_nnet_benchmarks-", format(Sys.time(), "%H:%M:%S-%d-%m-%Y"),".Rdata",sep=""))
# 
# ### predict kaggle data
# competition.preds <- predict(best.nnet,kaggle,type="class")
# 
# write.csv(data.frame(ImageId=1:28000,Label=competition.preds),
#           file="cv-pars_nn_submission.csv",row.names=F,quote=F)
