library(data.table)
library(caret)
library(doMC)
registerDoMC(cores=detectCores())
library(nnet)
library(RSNNS)

### load data
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}

### 1. find pixels with zero variance (dead)
digit.dt <- data.table(digit.data)
dead.pixels <- which(digit.dt[,!"label",with=F][,lapply(.SD,var)]==0) +1 #+1 due to subsetting

### 2. check that none of these are in rf_selected covariates
load("rf_selected_vars.Rdata")
selected.pixels <- which(colnames(digit.dt) %in% top.40.mdg.and.mda)
selected.pixels <- setdiff(selected.pixels,dead.pixels)

### 3. build training set
set.size <- 15000
### normalize input data
training.set.x <- RSNNS::normalizeData(digit.dt[1:set.size,selected.pixels,with=F])
training.set.y <- digit.dt[1:set.size,label]
training.set <- data.table(label=training.set.y, training.set.x) #nnet::class.ind()

### make test set
test.set.x <- RSNNS::normalizeData(digit.dt[41001:42000,selected.pixels,with=F])
test.set <- data.table(label=digit.data[41001:42000,1], test.set.x)

## prepare kaggle set
load(file="data_and_benchmarks/testing_data.Rdata")
kaggle <- data.table(RSNNS::normalizeData(unlabeled.digit.data[,selected.pixels-1]))

### make grid for .size
num.of.inputs <- length(selected.pixels)
num.of.outputs <- 10
num.of.nodes <- mean(c(num.of.inputs,num.of.outputs))

### create hyper-parameter grid
tuning.grid <- expand.grid(.size=c(12,19,26),.decay=c(0,0.1,1e-4),.bag=F)

### train with caret
ctrl <- trainControl(method="cv",number=10,repeats=3)
time.avNNet <- system.time(caret.avNNet <- caret::train(label~.,data=training.set, method="avNNet", tuneGrid=tuning.grid, trControl=ctrl, softmax=T, MaxNWts=12000))


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

best.nnet <- caret.avNNet$finalModel
acc.est <- estAccuracy(best.nnet,test.set,verbose=T)

save(tuning.grid, time.avNNet, caret.avNNet, acc.est, file=paste("parsimonious_nnet_results-", format(Sys.time(), "%H:%M:%S-%d-%m-%Y"),".Rdata",sep=""))

### predict kaggle data
competition.preds <- predict(best.nnet,kaggle,type="class")

write.csv(data.frame(ImageId=1:28000,Label=competition.preds),
          file="cv-pars_nn_submission.csv",row.names=F,quote=F)
