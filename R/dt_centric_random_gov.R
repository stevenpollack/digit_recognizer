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

n.committees <- 100

### build house within DT framework
set.seed(1234)
rand.rows <- sample(x=1:3200,size=10,replace=F)

test.set <- digit.data[,.SD[rand.rows],by=label]
test.set.response <- test.set[,label]
test.set[,label:=NULL]

system.time(random.house <- digit.data[,.SD[sample(x=.N, size=n.committees)],by=label][,`:=`(committee.num=1:n.committees,metascore=0)])

### key random.house by committee number
### so we can loop through DT in parallel
setkey(random.house,committee.num)

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

trainRandomHouse <- function(training.set.input, training.set.response, random.house) {
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
    
    return(winner)
  })
  return(predicted.values)
}

system.time(out <- trainRandomHouse(test.set,test.set.response,random.house))
confusion.mat <- table(ordered(unlist(out),0:9),test.set.response)
accuracy.est <- sum(diag(confusion.mat))/length(test.set.response); accuracy.est



### 15 minutes to train on 1k subjects, with 87.3% vanilla voting accuracy


### train meta-score
trained <- 

votes[,.N,by=label][N==max(N),sample(x=label,size=1)]}



############## benchmark for ways to predict/train
microbenchmark(voting <- random.house[, {
  distances <- apply(t(.SD[,!"label",with=F][,!"metascore",with=F]),2,function(representative){sqEucDist(representative,test.citizen)})
  class.estimate <- as.factor(0:9)[which.min(distances)]
  }, by=committee.num][,votes:=.N,by=V1][votes==max(votes),V1][],times=10)

microbenchmark(random.house[, list(label,apply(t(.SD[,!"label",with=F][,!"metascore",with=F]),2,function(representative){sqEucDist(representative,test.citizen)})), by=committee.num][,label[which.min(V2)],by=committee.num][,.N,by=V1][N==max(N),sample(x=V1,size=1)],times=10) # ~ 10 seconds

microbenchmark(, times=10) #~ 2 seconds



buildCommittee <- function(potential.representatives=digit.data, according.to="label") {
  require(data.table)
  ### potential.representatives is the data.table with column "according.to"
  ### from (by) which 1 random representative will be chosen. In the government
  ### analogy, we want to build a committee that has 1 member from every level of
  ### "according.to". So, in effect, according.to holds the "party affiliation"
  ### of every potential represenative.
  
  ### for speed, assume that potentital.representatives is key'd by "according.to".
  potential.representatives[,{
    size.of.group <- .N
    representative.number <- sample(x=size.of.group,size=1)
    .SD[representative.number]
  }, by=eval(according.to)]
}

buildHouse <- function(n.committees=10, potential.representatives=digit.data, according.to="label") {
  require(plyr)
  ### naive implementation returns the "House" as a list of n.committees 
  llply(.data=(1: n.committees),.fun=function(dummy.var){buildCommittee(potential.representatives,according.to)})
}

sqEucDist <- function(x,y) { sum((x-y)^2) }

assembleCommittee <- function(committee, citizen, according.to="label", dist=sqEucDist) {
  #require(data.table)
  ### bring a committee together to consider the "party" of any citizen
  ### The citizen must have same structure as a representative.
  ### Prediction is based on the "party" of the committee member that
  ### lies closest (default is euclidean distance^2) to the citizen.
  
  ### this implementation currently assumes one member per party in the committee
  ### and that citizen is unlabeled
  committee[,"distance":=dist(.SD,citizen),by=eval(according.to)][which.min(distance),evalq(according.to),with=F]
  
  
}

assembleHouse <- function(house, citizen, according.to="label", dist=sqEucDist, train=F) {
  require(data.table); require(plyr)
  ### assemble each committee in the house to deliberate on a citizen
  ### modify seniority if train=T
  data.table(ldply(.data=house,.fun=function(committee) {
    assembleCommittee(committee, citizen, according.to, dist)
  }))
}

processVote <- function(vote.results) {
  require(data.table)
  ### this is naive and doesn't implement a meta-score for tie breaker
  ### arbitrarily chose winner of a tie.
  vote.results[,votes:=.N,by=label][votes==max(votes),sample(x=label,size=1)]
}
