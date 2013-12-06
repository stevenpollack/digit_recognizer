### libraries for SVM
library(e1071) ### for classAgreement()
library(kernlab) ### for ksvm 

### library for parallel computation
library(doMC)
registerDoMC(cores=detectCores()) 
library(plyr)

### libraries for visualization
library(ggplot2)
library(reshape2)

### load data
### first column has digit label
### -----------------------------
digit.data <- read.csv(file="data_and_benchmarks/train.csv",header=T)
rownames(digit.data) <- paste("image",1:42000,sep="")
digit.data[,1] <- as.factor(digit.data[,1])
### tranpose data for faster manipulation
t.digit.data <- t(digit.data)
### ----------------------------

### visualize first 15 images
### ---------------------
llply(.data=1:15,.fun=function(index){
  ### digits are 28px x 28px images
  sample.digit <- matrix(data=as.numeric(t.digit.data[-1,index]),nrow=28,ncol=28)
  plot.title <- paste("image", index, sep="")
  ggplot(data=melt(sample.digit,varnames=c("X","Y")), aes(x=X,y=-Y,fill=value),show_guide=F) + geom_tile() + labs(x="",y="",title=plot.title)  
})
### -----------------------

sigma.est <- sigest(label~.,data=digit.data) # median at 1.47e-7


training.set <- digit.data[1:150,]
test.set <- digit.data[751:1500,]

# system.time(test.svm <- svm(label~., data=training.set, cost=10, cross=10))
# test.predictions <- predict(test.svm, test.set)
# table(test.predictions, test.set[,1])

### find a good C value:
cost.seq <- seq(from=1,to=100,length.out=25)
cv.seq <- laply(.data=cost.seq[10],.parallel=T,
                .fun=function(cost){
  tmp.svm <- ksvm(label~., data=training.set, cross=10, kpar=list(sigma=sigma.est[2]), C=cost)
  attr(x=tmp.svm,which="cross")
})

### train three svms in parallel
par.svms <- llply(.data=list(1:750,751:1500,1501:2250),.parallel=T,.fun=function(data.subset){
  ksvm(label~., data=digit.data[data.subset,], kpar=list(sigma=sigma.est[2]), C=15)
})

test.set <- digit.data[3000:4000,]
accuracies <- llply(.data=par.svms,.parallel=T,.fun=function(svm.model){
  classAgreement(table(predict(svm.model, test.set), test.set[,1]))
})
# diag:
# 0.9240759 0.8931069 0.8931069
# mean = 0.903

system.time(test.svm2 <- ksvm(label~.,data=training.set,cross=10,C=cost.seq[which.min(cv.seq)],kpar=list(sigma=1e-7)))
test.predictions2 <- predict(test.svm2, test.set)

declareWinner <- function(vote.table) {
  # check for clear winner
  max.vote <- max(vote.table)
  if (length(which(vote.table == max.vote)) == 1) {
    return(names(vote.table)[which.max(vote.table)])
  } else { # we've got a tie, so randomly choose
    candidates <- names(vote.table)[which(vote.table == max.vote)]
    return(sample(x=candidates,size=1))
  }
}

### (T,F):
###   user  system elapsed 
### 575.712   7.552 689.146 
### (F,T):
###     user   system  elapsed 
### 1750.140  475.664 1168.451 

system.time(mass.prediction <- alply(.data=test.set,.margins=1,.parallel=F,.fun=function(image.data){
  preds <- llply(.data=par.svms,.parallel=T,.fun=function(svm.model){
    predict(svm.model,image.data)
  })
  preds <- table(unlist(preds))
  declareWinner(preds)
}))

classAgreement(table(unlist(mass.prediction),test.set[,1]))
### diag: 0.9101

### look into overhead of partitioning training set and training multiple
### svms, and voting,

### save half of training set for testing purposes
length.of.training.set <- 750
training.set.boundaries <- rbind(seq(from=1,to=21000,by=length.of.training.set),seq(from=0,to=21000,by=length.of.training.set)[-1])
test.set <- digit.data[21001:42000,]

system.time(svm.collection <- alply(.data=training.set.boundaries,.margins=2,.parallel=T,.fun=function(boundary){
  data.seq <- boundary[1]:boundary[2]
  ksvm(label~., data=digit.data[data.seq,], kpar=list(sigma=sigma.est[2]), C=15)
}))

system.time(individual.accuracies <- llply(.data=svm.collection,.parallel=T,.fun=function(svm.model){
  classAgreement(table(predict(svm.model, test.set[1:1000,]), test.set[1:1000,1]))
}))

system.time(mass.prediction <- alply(.data=test.set[1:1000,],.margins=1,.parallel=T,.fun=function(image.data){
  preds <- llply(.data=svm.collection,.parallel=F,.fun=function(svm.model){
    predict(svm.model,image.data)
  })
  preds <- table(unlist(preds))
  declareWinner(preds)
}))
# user  system elapsed 
# 484.364  36.140 600.651 

classAgreement(table(unlist(mass.prediction),test.set[1:1000,1]))