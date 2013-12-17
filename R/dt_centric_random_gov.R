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

sqEucDist <- function(x,y) { sum((x-y)^2) }

committeePredict.old <- function(random.house, committee.number, test.citizen) {
  random.house[J(committee.number),
               sqEucDist(.SD[,!"metascore",with=F][,!"committee.num",with=F],test.citizen)
               ,by=label][,label[which.min(V1)]]
}

pixel.cols <- colnames(random.house)[which(!colnames(random.house) %in% c("label","metascore","committee.num"))]

committeePredict <- function(random.house, committee.number, test.citizen) {
  ### from ?dist, accessing distance from test.citizen to committee.member.k
  ### is from dist()[(i-1)*(n-i/2) + j-i] where n = 11, i = k, j = 11
  distances <- dist(rbind(random.house[J(committee.number),pixel.cols,with=F],test.citizen))
  distances <- distances[(1:10-1)*(11-1:10/2) + 11-(1:10)]
  as.factor(0:9)[which.min(distances)]
}
### look into modifying train to simultaneously gauge vanilla accuracy.

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

benchmarkRG <-function(test.class.size=10,num.of.committees=100,verbose=F) {
  set.seed(1234)
  n.committees <- num.of.committees
  
  # build test set.
  test.set <- digit.data[digit.data[,sample(x=.I[1]:(.I[1]+.N),size=test.class.size),by=label][,V1]]
  test.set.response <- test.set[,label]
  test.set[,label:=NULL]
  
  if (verbose)  print(paste("built test set of dimension", dim(test.set)) )
  
  # build random.house
  random.house <- digit.data[,.SD[sample(x=.N, size=n.committees)],by=label][,`:=`(committee.num=1:n.committees,metascore=0)]
  setkey(random.house,committee.num)
  
  if (verbose) print("built house")
  # benchmark
  train.time <- system.time(out <- trainRandomHouse(test.set,test.set.response,random.house, n.committees, verbose=T))
  confusion.mat <- table(out,test.set.response)
  accuracy.est <- sum(diag(confusion.mat))/length(test.set.response)
  
  return(list(randomHouse=random.house,predictions=out,confMat=confusion.mat,accEst=accuracy.est))
}

par.grid <- c(3,5,7,21,35,56,75,101,123,151)
results <- lapply(X=par.grid,FUN=function(num.of.committees) {
  benchmarkRG(100,num.of.committess,verbose=T)  
})

save(par.grid,results,file="rg_benchmark_15h26.Rdata")

### 15 minutes to train on 1k subjects, with 87.3% vanilla voting accuracy

