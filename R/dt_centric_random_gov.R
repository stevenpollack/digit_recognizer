library(data.table)
library(doParallel)
registerDoParallel(cores=detectCores())

### load data
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}
# key on label
setkey(x=digit.data,label)
pixel.cols <- colnames(digit.data)[which(!colnames(digit.data) %in% c("label"))]

committeePredict <- function(random.house, committee.number, test.citizen, method="euclidean") {
  ### do some defensive programming here.
  
  ### from ?dist, accessing distance from test.citizen to committee.member.k
  ### is from dist()[(i-1)*(n-i/2) + j-i] where n = 11, i = k, j = 11
  distances <- dist(rbind(random.house[J(committee.number),pixel.cols,with=F],test.citizen),method=method)
  distances <- distances[(1:10-1)*(11-1:10/2) + 11-(1:10)]
  as.factor(0:9)[which.min(distances)]
}
vanillaWinner <- function(class.votes) {
  ### break tie by randomly guessing
  class.votes[,.N,keyby=predicted.class][N==max(N),sample(predicted.class,1)]
}
metascoreWinner <- function(random.house,class.votes) {
  ### DT schema:
  ### random.house[couple class votes and metascore][synthetically bolster class votes][perform vanilla winner calculation]
  ### ties are broken arbitrarily... will look into something better later.
  random.house[,class.votes[committee.num],by=c("committee.num","metascore")][,rep(predicted.class,metascore),by=committee.num][,.N,keyby=V1][N==max(N),sample(V1,1)]
}

trainRandomHouse <- function(training.set.input, training.set.response, random.house, n.committees, method="euclidean", verbose=F) {
  ### assuming trainingset has structure == obs x covariates
  ### and that training.set.respose[i] = class(training.set.input[i])
  
  size.of.training.set <- length(training.set.response)
  training.set.input.t <- t(training.set.input) # to speed up apply()
  predicted.values <- lapply(X=1:size.of.training.set,FUN=function(i) {
    
    test.citizen <- training.set.input[i]
    test.citizen.class <- training.set.response[i]
    
    # do parallel committee classification and metascore calculation
    class.and.score <- foreach(committee.number=1:n.committees) %dopar% {
      predicted.class <- committeePredict(random.house, committee.number, test.citizen, method)
      ### update metascore if prediction is good.
      score.update <- if (predicted.class == test.citizen.class) {
        rep(1,10)
      } else {
        rep(0,10)
      }
      return(list(predicted.class=predicted.class,score=score.update))
    }
    # unpack results
    metascore.update <- unlist(lapply(class.and.score,function(list){list$score}))
    class.votes <- rbindlist(lapply(class.and.score,function(list){data.table(predicted.class=list$predicted.class)}))
    
    # update random.house's metascore
    random.house[,metascore:=metascore+metascore.update]
    
    # determine a winner 
    winner <- class.votes[,.N,keyby=predicted.class][N==max(N),sample(predicted.class,1)][]
    if (verbose) print(paste("Predicted class", winner))
    return(winner)
  })
  
  return(ordered(unlist(predicted.values),0:9))
}

testRandomHouse <- function(test.set.input, test.set.response, random.house, n.committees, method="euclidean", verbose=F) {
  ### assuming test.set has structure == obs x covariates
  ### and that training.set.respose[i] = class(training.set.input[i])
  
  size.of.test.set <- length(test.set.response)
  test.set.input.t <- t(test.set.input) # to speed up apply()
  test.time <- system.time( predicted.values <- apply(X=test.set.input.t,MARGIN=2,
                                                      FUN=function(test.citizen) {
                                                        # do parallel committee classification
                                                        test.citizen <- data.table(matrix(test.citizen,nrow=1,dimnames=list(1,names(test.citizen))))
                                                        class.votes <- foreach(committee.number=1:n.committees,.final=rbindlist) %dopar%
{
  data.table(predicted.class=committeePredict(random.house, committee.number,test.citizen, method))
}
                                                        
                                                        # determine winners
                                                        winners <- data.table(vanilla=vanillaWinner(class.votes),metascore=metascoreWinner(random.house,class.votes))
                                                        
                                                        if (verbose) print(winners)
                                                        return(winners)
                                                      }))
  
  out <- rbindlist(predicted.values)
  out[,`:=`(vanilla=ordered(vanilla,0:9),metascore=ordered(metascore,0:9))]
  
  ts.len <- length(test.set.response)
  
  vanilla.conf.mat <- table(out[,vanilla],test.set.response)
  metascore.conf.mat <- table(out[,metascore],test.set.response)
  
  vanilla.accuracy <- sum(diag(vanilla.conf.mat))/ts.len
  metascore.accuracy <- sum(diag(metascore.conf.mat))/ts.len
  
  if (verbose) {print(data.table(vanilla.acc=vanilla.accuracy,ms.acc=metascore.accuracy))}
  
  return(list(predictions=out,time=test.time,vanilla=list(confMat=vanilla.conf.mat,acc.est=vanilla.accuracy),metascore=list(confMat=metascore.conf.mat,acc.est=metascore.accuracy)))
}

benchmarkRG <-function(training.samples.per.class=10, test.samples.per.class=10,num.of.committees=10, method="euclidean", verbose=F) {
  set.seed(1234)
  
  # build training and test sets (can do this in one operation with labels)
  # do it with 2 for clarity.
  training.set <- digit.data[digit.data[,sample(x=.I[1]:(.I[1]+.N),size=training.samples.per.class),by=label][,V1]]
  training.set.response <- training.set[,label]
  training.set[,label:=NULL]
  
  # default test on 100 examples
  test.set <- digit.data[digit.data[,sample(x=.I[1]:(.I[1]+.N),size=test.samples.per.class),by=label][,V1]]
  test.set.response <- test.set[,label]
  test.set[,label:=NULL]
  
  if (verbose)  print(paste("built test set of dimension", dim(test.set)) )
  
  # build random.house
  random.house <- digit.data[,.SD[sample(x=.N, size=num.of.committees)],by=label][,`:=`(committee.num=1:num.of.committees,metascore=0)]
  setkey(random.house,committee.num)
  
  if (verbose) print("built house")
  # benchmark
  train.time <- system.time(train.out <- trainRandomHouse(training.set,training.set.response,random.house, num.of.committees, method, verbose))
  train.confusion.mat <- table(train.out,training.set.response)
  train.accuracy.est <- sum(diag(train.confusion.mat))/length(training.set.response)
  
  train.list <- list(predicted.values=train.out, time=train.time, confMat=train.confusion.mat, accEst=train.accuracy.est)
  
  test.list <- testRandomHouse(test.set,test.set.response,random.house,num.of.committees,method,verbose)
  return(list(randomHouse=random.house,train=train.list,test=test.list))
}

par.grid <- expand.grid(training.samples.per.class=c(1,3,5,7),num.of.committees=c(3,5,7,11,21,35,56,77,98,119,255,401,512)))

results <- apply(X=par.grid,1,FUN=function(params) {
  training.samples.per.class <- params[1]
  num.of.committees <- params[2]
  print(paste("starting computation for", training.samples.per.class, "ts.p.c and ", num.of.committees, "committees"))  
  benchmarkRG(training.samples.per.class=training.samples.per.class,test.samples.per.class=100, num.of.committees=num.of.committees,method="binary")
})

save(par.grid,results,file="binary_dist_rg_benchmark_01h42.Rdata")