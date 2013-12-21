### look into knn-based approach.
library(microbenchmark)
library(data.table)
library(class)
library(doParallel)
registerDoParallel(cores=detectCores())

### load data
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}

pixel.cols <- colnames(digit.data)[which(!colnames(digit.data) %in% c("label"))]

### subset original data and randomly split up subset into test and train and building subsets

### note: for n committees, you need a building set of size at least 10*n.
### so that you can have 10 distinct classes per committee.
### You also want to have a test set of size >= 1k for decent 
### estimation of accuracy. If you want to train, you should also try and
### make a subset that has at least 3 samples from every class.
### Thus, make sure your subset is at easily bigger than 1k + 10*n + 30.
set.seed(1234)
subset.size <-10000 # should be greater than 1000
subset.indices <- sample(seq.int(nrow(digit.data)), subset.size, replace=F)
digit.subset <- digit.data[subset.indices]
setkey(digit.subset,label)

test.set.indices <- sample(seq.int(subset.size),1000,replace=F)
test.set <- digit.subset[test.set.indices]
training.set <- digit.subset[-test.set.indices]

### separate building set from within training sets.
building.set.indices <- training.set[,sample(x={.I[1]:.I[.N]},size=10,replace=F),by=label][,V1]
building.set <- training.set[building.set.indices]
training.set <- training.set[-building.set.indices]

### separate test and training set input and response
test.set.response <- test.set[,label]
test.set[,label:=NULL]

training.set.response <- training.set[,label]
training.set[,label:=NULL]

num.of.committees <- 10

buildRandomGov <- function(num.of.committees=10, digit.data) {
  setkey(digit.data,label)
  ### build random gov then store committees in a list
  random.house <- digit.data[,.SD[sample(x=.N, size=num.of.committees)],by=label][,`:=`(committee.num=1:num.of.committees)]
  setkey(random.house,committee.num)
  
  ### store random.committees in a list: ( ~ 252 ms)
  random.gov <- lapply(X=1:num.of.committees, FUN=function(i) {
    list(committee=as.matrix(random.house[J(i),pixel.cols,with=F]),labels=random.house[J(i),"label",with=F][,label])
  })
  return(random.gov)
}

### benchmark a few strategies
################
# ### ~ 122 ms
# microbenchmark(out1 <- matrix(unlist(lapply(X=random.gov, FUN=function(rg.com) {knn(train=rg.com$committee,test=test.set,cl=rg.com$labels,k=1)})),nrow=10,byrow=T),times=10)
# 
# ### ~ 129 ms
# microbenchmark(out2 <- matrix(foreach(rg.com=iter(random.gov),.final=unlist) %do% {(knn(train=rg.com$committee,test=test.set,cl=rg.com$labels,k=1))},nrow=10,byrow=T),times=10)
# 
# ### ~ 131 ms
# microbenchmark(out3 <- matrix(foreach(i=seq.int(num.of.committees),.final=unlist) %do% {knn(train=random.gov[[i]]$committee,test=test.set,cl=random.gov[[i]]$labels,k=1)},nrow=10,byrow=T),times=10)
# ##############

trainRandomGov <- function(random.gov,training.set,training.set.response,method="prob") {
  ### synthetic method articially inflates counts according to how many
  ### times a committee had the right member closest to a training example.
  ### probability method scales the above count to yield an in-class accuracy
  ### estimate.
  
  ### use kNN to block-train random.government metascores (~ 126ms)
  metascores <- if (method=="synthetic") {
    lapply(X=random.gov,FUN=function(rg.com){
      predictions <- knn(train=rg.com$committee,
                         test=training.set,cl=rg.com$labels,k=1)
      table(predictions[predictions == training.set.response])
    }) 
  } else if (method=="prob") {
    lapply(X=random.gov, FUN=function(rg.com){
      predictions <- knn(train=rg.com$committee,
                         test=training.set,cl=rg.com$labels,k=1)
      counts <- table(predictions[predictions == training.set.response])
      proportions <- counts / table(predictions)
      return(proportions) # which.max will ignore NaN's from 0/0
    })
  }
   
  ### store metascore table back inside random.gov (~ 1.29 ms)
  random.gov <- lapply(X=1:num.of.committees,FUN=function(i){
    random.gov[[i]]$metascore <- metascores[[i]]
    random.gov[[i]]
  })
  return(random.gov)  
}

### help functions for predict functionality
########

vanillaWinner <- function(predictions) {
  vote.table <- table(predictions)
  candidates <- dimnames(vote.table)$predictions
  winners <- candidates[which(vote.table == max(vote.table))]
  winner <- as.factor(sample(x=winners,size=1))
  return(ordered(winner,as.factor(0:9)))
}

### synthetical bolster predictions.mat
rescalePredictions <- function(predictions,random.gov) {
  ### random.gov[[i]]$committee's vote == predictions[i]
  synthetic.votes <- sapply(X=seq.int(length(predictions)),
         FUN=function(committee.num){
           predicted.class <- predictions[committee.num]
           rep(predicted.class,random.gov[[committee.num]]$metascore[predicted.class])
         })
  return(unlist(synthetic.votes))
}
syntheticMetascoreWinner <- function(predictions,random.gov) { 
  vanillaWinner(rescalePredictions(predictions,random.gov))
}

### choose winner based on highest likelihood
probMetaScoreWinner <- function(predictions,random.gov) {
  ### random.gov[[i]]$committee's vote == committee.predictions[i]
  candidate.certainties <- sapply(X=seq.int(length(predictions)),
                            FUN=function(committee.num){
                              predicted.class <- predictions[committee.num]
                              random.gov[[committee.num]]$metascore[predicted.class]
                            })
  return(sample(predictions,size=1,prob=candidate.certainties))
}

processElections <- function(predictions.mat, random.gov, method="vanilla"){
  out <- if (method=="vanilla") {
    apply(X=predictions.mat,MARGIN=2,FUN=vanillaWinner) 
  } else if (method=="synthetic") {
    apply(X=predictions.mat,MARGIN=2,FUN=function(committee.predictions,random.gov){syntheticMetascoreWinner(committee.predictions,random.gov)},random.gov)
  } else if (method=="prob") {
    apply(X=predictions.mat,MARGIN=2,FUN=function(committee.predictions,random.gov){probMetaScoreWinner(committee.predictions,random.gov)},random.gov)
  }
  return(out)
}
########

predictRandomGov <- function(random.gov,test.set,method="vanilla") {
  ### use kNN to block process test set and aggregate predictions
  predictions <- lapply(X=random.gov, FUN=function(rg.com) {knn(train=rg.com$committee,test=test.set,cl=rg.com$labels,k=1)}) 
  ### process results into matrix
  predictions.mat <- matrix(unlist(predictions),nrow=10,byrow=T)
  ### process elections
  return(processElections(predictions.mat,random.gov,method))
}

estimatePerformance <- function(predicted.classes,actual.classes) {
  conf.mat <- table(predicted.classes,actual.classes)
  acc.est <- sum(diag(conf.mat))/length(actual.classes)
  return(list(confusionMatrix=conf.mat, accuracyEst=acc.est))
}

### ~ 9.65 s for 10 committees, 3.9k obs training set and 1k test set
buildTrainPredict <- function(num.of.committees,building.set,training.set,training.set.response,test.set,test.set.response,method="vanilla") {
  random.gov <- buildRandomGov(num.of.committees,building.set)
  random.gov <- trainRandomGov(random.gov,training.set,training.set.response,method=method)
  predictions <- predictRandomGov(random.gov,test.set,method=method)
  performance <- estimatePerformance(predictions,test.set.response)
  list(predictions=predictions, performance=performance, randomGov=random.gov)
}

timing <- microbenchmark(out <- buildTrainPredict(num.of.committees=10,building.set,training.set,training.set.response,test.set,test.set.response,method="prob"),times=1)

### notes:
### it seems that training set size can quickly lead to over-fitting.
### I suspect there is a direct relationship between num.of.committees and
### accuracy when vanilla voting is implemented. Also, results seem very 
### dependent on distance used.