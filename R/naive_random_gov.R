library(data.table)

### load data
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}
# key on label
setkey(x=digit.data,label)

n.committees <- 7

### naive list-based implementation
library(plyr)

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


system.time(test.house <- buildHouse(n.committees=n.committees))
test.committee <- copy(test.house[[1]])
house.votes <- assembleHouse(house=test.house,citizen=test.citizen)
results <- processVote(house.votes) 

test.set <- digit.data[, .SD[1], by="label"]
test.set.response <- test.set[,label]

### take away: apply over t(test.set), then foreach(1:n.committees, .final=rbindlist)
### is fastest. consider house as a list of dt's with shallow copy:
### http://stackoverflow.com/questions/17133522/invalid-internal-selfref-in-data-table
### OR just a subset with labels.




system.time(
test.set.preds <- llply(.data=1:10,.fun=function(i){
  house.votes <- assembleHouse(house=test.house,citizen=test.set[i,!"label",with=F])
  processVote(house.votes)
})
)

digit.data[1:100,list(label)]
table(unlist(test.set.preds),test.set.response)

library(doParallel)
registerDoParallel(cores=8)

test.citizen <- test.set[2,!"label",with=F]
system.time(foreach(test.citizen=iter(test.set[,!"label",with=F],by="row")) %do% {
  foreach(committee=iter(test.house,by="cell"),.combine="rbind",.final=data.table) %dopar% {
    timers <- system.time(result <- assembleCommittee(committee, test.citizen))
    data.frame(vote=result,time=timers[3])
  }
})

system.time(t(digit.data))

system.time(foreach(row=iter(digit.data[1:1000],by="row")) %do% {x<-2})
system.time(apply(digit.data[1:1000],1,FUN=function(row){x<-2}))

system.time(foreach(row=iter(t(digit.data[1:1000]),by="column")) %do% {x<-2})
system.time(apply(t(digit.data[1:1000]),2,FUN=function(row){x<-2}))

system.time(foreach(committee=iter(test.house,by="cell"),.combine="rbind",.final=data.table) %dopar% {
  timers <- system.time(result <- assembleCommittee(committee, test.citizen))
  data.frame(vote=result,time=timers[3])
})

system.time(apply(X=t(test.set[,!"label",with=F]),MARGIN=2,FUN=function(test.citizen) {
  foreach(i=1:n.committees,.combine=rbind,.final=data.table) %dopar% {
    committee <- copy(test.house[[i]])
    timers <- system.time(result <- assembleCommittee(committee, test.citizen))
    data.frame(vote=result,time=timers[3])
  }
}))

microbenchmark( o1<- foreach(i=1:n.committees,.combine=rbind,.final=data.table) %dopar% {
  committee <- copy(test.house[[i]])
  timers <- system.time(result <- assembleCommittee(committee, test.citizen))
  data.frame(vote=result,time=timers[3])
},  o2 <- foreach(i=1:n.committees,.final=data.table::rbindlist) %dopar% {
  committee <- copy(test.house[[i]])
  timers <- system.time(result <- assembleCommittee(committee, test.citizen))
  data.table(vote=result,time=timers[3])
}, times=10)
### apply over columns is still fastest

