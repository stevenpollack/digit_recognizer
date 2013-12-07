### this script is set up to test the speed of training
### as well as accuracy of the linear vs. Gaussian kernel.
### Protocol:
### train on overlapping subsets of digit.data
### and then test on 1000 fresh observations

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

sample.sizes <- c(200*2^(0:5),10000,200*2^6,15000,20000,28000,35000,40000)
test.set <- digit.data[40001:42000,]

### test non-cross-validated Gaussian
test1.time <- system.time(speed.test <- llply(.data=sample.sizes,.parallel=T,.fun=function(sample.size){
  training.time <- system.time(svm.model <- ksvm(label~., data=digit.data[1:sample.size,], C=15))
  list(svm=svm.model, time=training.time)
}))

### test non-cross-validated Linear
test2.time <- system.time(speed.test2 <- llply(.data=sample.sizes,.parallel=T,.fun=function(sample.size){
  training.time <- system.time(svm.model <- ksvm(label~., data=digit.data[1:sample.size,], kernel="vanilladot", C=15))
  list(svm=svm.model, time=training.time)
}))


gaussian.speed.results <- unlist(llply(.data=speed.test,.fun=function(results){results$time[3]}))

linear.speed.results <- unlist(llply(.data=speed.test2,.fun=function(results){results$time[3]}))

test.speed.results <- data.frame(sample.size=sample.sizes,Gaussian=gaussian.speed.results,Linear=linear.speed.results,stat="speed")

gaussian.accuracy.results <- laply(.data=speed.test,.parallel=T,.fun=function(results){
  svm.model <- results$svm
  predictions <- predict(svm.model,test.set,type="response")
  matrix(classAgreement(table(predictions, test.set[,1])),nrow=1)
})

linear.accuracy.results <- laply(.data=speed.test2,.parallel=T,.fun=function(results){
  svm.model <- results$svm
  predictions <- predict(svm.model,test.set,type="response")
  matrix(classAgreement(table(predictions, test.set[,1])),nrow=1)
})

test.accuracy.results <- data.frame(sample.size=sample.sizes,Gaussian=unlist(gaussian.accuracy.results[,1]),Linear=unlist(linear.accuracy.results[,1]),stat="accuracy")

test.results <- rbind(test.speed.results,test.accuracy.results)

test.results.plot <- ggplot(data=melt(test.results,id.vars=c("sample.size","stat")), aes(x=sample.size,y=value,color=variable,group=stat:variable)) + geom_point() + geom_line() + facet_grid(facets=stat~.,scales="free_y")

workspace.vars <- ls()[which(!ls() %in% c("digit.data","training.set","test.set"))]

save(list=workspace.vars, file=paste("speed-test_results-", format(Sys.time(), "%H:%M:%S-%d-%m-%Y"),".Rdata",sep=""))

### take away: marginal returns after training set exceeds 10k.
