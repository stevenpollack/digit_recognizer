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

# n.committees <- 100
# 
# ### build house within DT framework
# set.seed(1234)
# rand.rows <- sample(x=1:3200,size=10,replace=F)
# 
# test.set <- digit.data[,.SD[rand.rows],by=label]
# test.set.response <- test.set[,label]
# test.set[,label:=NULL]
# 
# system.time(random.house <- digit.data[,.SD[sample(x=.N, size=n.committees)],by=label][,`:=`(committee.num=1:n.committees,metascore=0)])
# 
# ### key random.house by committee number
# ### so we can loop through DT in parallel
# setkey(random.house,committee.num)

# sqEucDist <- function(x,y) { sum((x-y)^2) }
# 
# committeePredict.old <- function(random.house, committee.number, test.citizen) {
#   random.house[J(committee.number),
#                sqEucDist(.SD[,!"metascore",with=F][,!"committee.num",with=F],test.citizen)
#                ,by=label][,label[which.min(V1)]]
# }

committeePredict <- function(random.house, committee.number, test.citizen) {
  ### do some defensive programming here.
  
  ### from ?dist, accessing distance from test.citizen to committee.member.k
  ### is from dist()[(i-1)*(n-i/2) + j-i] where n = 11, i = k, j = 11
  distances <- dist(rbind(random.house[J(committee.number),pixel.cols,with=F],test.citizen))
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

trainRandomHouse <- function(training.set.input, training.set.response, random.house, n.committees, verbose=F) {
  ### assuming trainingset has structure == obs x covariates
  ### and that training.set.respose[i] = class(training.set.input[i])
  
  size.of.training.set <- length(training.set.response)
  training.set.input.t <- t(training.set.input) # to speed up apply()
  predicted.values <- lapply(X=1:size.of.training.set,FUN=function(i) {
    
    test.citizen <- training.set.input[i]
    test.citizen.class <- training.set.response[i]
    
    # do parallel committee classification and metascore calculation
    class.and.score <- foreach(committee.number=1:n.committees) %dopar% {
      predicted.class <- committeePredict(random.house, committee.number, test.citizen)
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

testRandomHouse <- function(test.set.input, test.set.response, random.house, n.committees, verbose=F) {
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
  data.table(predicted.class=committeePredict(random.house, committee.number,test.citizen))
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

################
# workshop
###############
# set.seed(1234)
# 
# training.set <- digit.data[digit.data[,sample(x=.I[1]:(.I[1]+.N),size=samples.per.class),by=label][,V1]]
# training.set.response <- training.set[,label]
# training.set[,label:=NULL]
# test.set <- digit.data[digit.data[,sample(x=.I[1]:(.I[1]+.N),size=samples.per.class),by=label][,V1]]
# test.set.response <- test.set[,label]
# test.set[,label:=NULL]
# 
# random.house <- digit.data[,.SD[sample(x=.N, size=num.of.committees)],by=label][,`:=`(committee.num=1:num.of.committees,metascore=0)]
# setkey(random.house,committee.num)
# 
# out <- trainRandomHouse(training.set,training.set.response,random.house, num.of.committees)
# 
# class.votes <- rbindlist(lapply(1:3,function(i){data.table(predicted.class=committeePredict(random.house,i,test.citizen))}))

system.time(lapply(X=1:100,function(i){x<-test.set[i];x<-2}))
system.time(apply(test.set.input.t,2,function(test.citizen){x <- data.table(matrix(test.citizen,nrow=1,dimnames=list(1,names(test.citizen)))); x<-2}))


################

benchmarkRG <-function(training.samples.per.class=10, test.samples.per.class=10,num.of.committees=10,verbose=F) {
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
  train.time <- system.time(train.out <- trainRandomHouse(training.set,training.set.response,random.house, num.of.committees, verbose=verbose))
  train.confusion.mat <- table(train.out,training.set.response)
  train.accuracy.est <- sum(diag(train.confusion.mat))/length(training.set.response)
  
  train.list <- list(predicted.values=train.out, time=train.time, confMat=train.confusion.mat, accEst=train.accuracy.est)

  test.list <- testRandomHouse(test.set,test.set.response,random.house,num.of.committees,verbose)
  return(list(randomHouse=random.house,train=train.list,test=test.list))
}



### testRH is slightly faster than trainRH because of the apply over the transpose.
#####
# library(microbenchmark)
# microbenchmark(,times=10)
# microbenchmark(out <- testRandomHouse(test.set[1:25],test.set.response[1:25],random.house,n.committees),times=5)
# {random.house2 <- digit.data[,.SD[sample(x=.N, size=n.committees)],by=label][,`:=`(committee.num=1:n.committees,metascore=0)]; setkey(random.house2,committee.num)}
# microbenchmark(out2 <- trainRandomHouse(training.set.input=test.set[1:25],training.set.response=test.set.response[1:25],random.house=random.house2,n.committees=n.committees),times=5)
#######

benchmarkRG(samples.per.class=3,num.of.committees=7,verbose=F)

par.grid <- expand.grid(training.samples.per.class=c(1,3,5,7),num.of.committees=c(3,5,7,11,21,35,56,77,98,119))

results <- apply(X=par.grid[1:2,],1,FUN=function(params) {
  training.samples.per.class <- params[1]
  num.of.committees <- params[2]
  print(paste("starting computation for", training.samples.per.class, "ts.p.c and ", num.of.committees, "committees"))  
  benchmarkRG(training.samples.per.class=training.samples.per.class,num.of.committees=num.of.committees)
})

save(par.grid,results,file="updated_rg_benchmark_16h35.Rdata")