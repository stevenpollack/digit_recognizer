library(caret)
library(doMC)
registerDoMC(cores=detectCores())
library(RSNNS)
library(data.table)

### check that data is loaded
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}

estAccuracy <- function(model,newdata, verbose=F,savePreds=F) {
  ### assuming the newdata.input is normalized and properly cleaned
  newdata.input <- newdata[,-1]
  newdata.output <- newdata[,1]
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

### do naive variable selection
### 1. find pixels with zero variance (dead)
digit.dt <- data.table(digit.data)
dead.pixels <- which(digit.dt[,!"label",with=F][,lapply(.SD,var)]==0) +1 #+1 due to subsetting
### 2. cycle through every other alive pixel
even.pixels <- seq(from=2,to=785,by=2)
selected.pixels <- even.pixels[(!even.pixels %in% dead.pixels)]

### build training set
set.size <- 15000
### normalize input data
training.set.x <- RSNNS::normalizeData(digit.data[1:set.size,selected.pixels])
training.set.y <- digit.data[1:set.size,1]
training.set <- data.frame(label=training.set.y, training.set.x) #nnet::class.ind()

### make test set
test.set.x <- RSNNS::normalizeData(digit.data[41001:42000,selected.pixels])
test.set <- data.frame(label=digit.data[41001:42000,1], test.set.x)

### make grid for .size
num.of.inputs <- length(selected.pixels)
num.of.outputs <- 10
num.of.nodes <- mean(c(num.of.inputs,num.of.outputs))/4

### create hyper-parameter grid
tuning.grid <- data.table(caret::createGrid(method="avNNet",len=4,data=training.set))
setkey(tuning.grid,.size)
tuning.grid[,.size:=round(num.of.nodes*.size)]

ctrl <- trainControl(method="cv",number=10,repeats=10)
time.avNNet <- system.time(caret.avNNet <- caret::train(label~.,data=training.set, method="avNNet", tuneGrid=tuning.grid, trControl=ctrl, softmax=T, MaxNWts=120000))

workspace.vars <- ls()[which(!ls() %in% c("digit.data","training.set", "digit.dt"))]
save(list=workspace.vars, file=paste("nnet_investigations_results-", format(Sys.time(), "%H:%M:%S-%d-%m-%Y"),".Rdata",sep=""))