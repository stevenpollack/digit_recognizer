library(nnet)
library(plyr)
library(doMC)
registerDoMC(cores=detectCores())

### check that data is loaded
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}
training.set <- digit.data[1:1000,]
test.set <- digit.data[41001:42000,]

trainAndTestNNet <- function(training.set,test.set,size.of.hidden.layer,verbose=F) {
  train.time <- system.time(digit.nn <- nnet(label~.,data=training.set,size=size.of.hidden.layer,MaxNWts=1000000,maxit=500))
  if (verbose) {print(train.time)}
  predicted.values <- predict(digit.nn, test.set[,-1], type="class")
  confusion.mat <- table(predicted.values, test.set[,1])
  if (verbose) {print(confusion.mat)}
  accuracy.est <- sum(diag(confusion.mat))/dim(test.set)[1]
  if (verbose) {print(accuracy.est)}
  list(nn=digit.nn,train.time=train.time,accuracy=accuracy.est)
}

nn.seq <- llply(.data=2^(1:8),.parallel=T,.inform=T,.fun=function(layer.size){
  trainAndTestNNet(training.set,test.set,layer.size,T)
})
