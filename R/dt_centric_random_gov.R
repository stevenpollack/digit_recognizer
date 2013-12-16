library(data.table)

### load data
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}
# key on label
setkey(x=digit.data,label)

n.committees <- 7

### build house within DT framework
set.seed(1234)
rand.rows <- sample(x=1:3200,size=5,replace=F)
test.set <- digit.data[rand.rows,!"label",with=F]
test.set.response <- digit.data[rand.rows,label]
system.time(random.house <- digit.data[,.SD[sample(x=.N, size=n.committees)],by=label][,`:=`(committee.num=1:n.committees,metascore=0)])

### key random.house by committee number
### so we can loop through DT in parallel
setkey(random.house,committee.num)

committeePredict <- function(random.house, committee.number, test.citizen) {
  random.house[J(committee.number),
               sqEucDist(.SD[,!"metascore",with=F][,!"committee.num",with=F],test.citizen)
               ,by=label][,label[which.min(V1)]]
}

trainRandomHouse <- function(training.set.input, training.set.response, random.house) {
  ### assuming trainingset has structure == obs x covariates
  ### and that training.set.respose[i] = class(training.set.input[i])
  size.of.training.set <- length(training.set.response)
  training.set.input.t <- t(training.set.input) # to speed up apply()
  .ignore <- lapply(X=1:size.of.training.set,FUN=function(i) {
    
    test.citizen <- training.set.input[i]
    test.citizen.class <- training.set.response[i]
    
    # do parallel committee classification and metascore calculation
    metascore.update <- foreach(committee.number=1:n.committees,.final=unlist) %dopar% {
      predicted.class <- committeePredict(random.house, committee.number, test.citizen)
      ### update metascore if prediction is good.
      if (predicted.class == test.citizen.class) {
        rep(1,10)
        } else {
          rep(0,10)
        }
    }
    random.house[,metascore:=metascore+metascore.update]
  })
}


random.house[,metascore:=0]
trainRandomHouse(test.set,test.set.response,random.house)



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
