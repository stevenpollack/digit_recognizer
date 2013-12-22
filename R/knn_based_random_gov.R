### look into knn-based approach.
library(microbenchmark)
library(data.table)
library(class)
library(doParallel)
registerDoParallel(cores=detectCores())

### --------------
### build routines
### --------------

buildRandomGov <- function(building.set, pixel.cols, num.of.committees=10, class.width=1) {
  set.seed(1234) # for reproducibility
  setkey(building.set,label)
  ### build random gov then store committees in a list
  committees.dt <- building.set[,.SD[sample(x=.N, size=class.width*num.of.committees,replace=TRUE)],by=label][,`:=`(committee.num=rep(1:num.of.committees,class.width))]
  setkey(committees.dt,committee.num)
  
  ### store random.committees in a list: ( ~ 252 ms)
  random.gov <- lapply(X=1:num.of.committees, FUN=function(i) {
    list(committee=as.matrix(committees.dt[J(i),pixel.cols,with=F]),labels=committees.dt[J(i),"label",with=F][,label])
  })
  return(random.gov)
}

selectivelyBuildRandomGov <- function(building.set, training.set, training.set.response, class.width=1, iter.limit=100, threshold=0.92, verbose=FALSE) {
  ### consider growing a committee for the specialized purpose
  ### of classifying label k, but allow the committee to have 
  ### multiple members from classes.
  require(doRNG) # allow for reproducible foreach loop
  set.seed(1234)
  build.time <- system.time(committee.search.results <- foreach(class=as.factor(0:9)) %dorng% {
    # initialize variables for while loop
    is.trained <- FALSE
    iter <- 0
    best.committee <- list(likelihood=0)
    while (iter < iter.limit) {
      potential.committee <- building.set[,.SD[sample(x=.N, size=class.width)],by=label]
      potential.committee.input <- potential.committee[,!"label",with=FALSE]
      potential.committee.response <- potential.committee[,label]
      predictions <- knn(train=potential.committee.input,
                         test=training.set,
                         cl=potential.committee.response,
                         k=1)
      counts <- table(predictions[predictions == training.set.response])
      proportions <- counts / table(predictions)
      if (proportions[class] > best.committee$likelihood) {
        best.committee$committee <- potential.committee.input
        best.committee$labels <- potential.committee.response
        best.committee$likelihood <- proportions[class]
        best.committee$metascore <- proportions
      }
      if (proportions[class] > threshold) {
        is.trained <- TRUE
        break
      } else {
        iter <- iter + 1
      }
    }
    list(isTrained=is.trained,bestCommittee=best.committee)
  })
  if (verbose) {
    cat("Random government took ", build.time[3], " seconds to build.\n")
  }
  if (verbose && !all(sapply(committee.search.results,function(list){list$isTrained}))) {
    cat("Warning: some committees were unable to train beyond threshold!\n")
  }
  random.gov <- lapply(committee.search.results,function(list){list$bestCommittee})
  if (verbose) {
    cat("Maximal likelihoods: \n", paste(lapply(random.gov,function(list){round(list$likelihood,3)}),collapse=", "))
  }
  return(random.gov)
}

### ---------------
### training routines
### ---------------

trainRandomGov <- function(random.gov,training.set,training.set.response,method="prob") {
  ### synthetic method articially inflates counts according to how many
  ### times a committee had the right member closest to a training example.
  ### probability method scales the above count to yield an in-class accuracy
  ### estimate.
  method <- match.arg(method, c("vanilla", "synthetic", "probabilistic"))
  ### use kNN to block-train random.government metascores (~ 126ms)
  metascores <- if (method=="synthetic") {
    lapply(X=random.gov,FUN=function(rg.com){
      predictions <- knn(train=rg.com$committee,
                         test=training.set,cl=rg.com$labels,k=1)
      table(predictions[predictions == training.set.response])
    }) 
  } else if (method=="probabilistic") {
    lapply(X=random.gov, FUN=function(rg.com){
      predictions <- knn(train=rg.com$committee,
                         test=training.set,cl=rg.com$labels,k=1)
      counts <- table(predictions[predictions == training.set.response])
      proportions <- counts / table(predictions)
      return(proportions) # which.max will ignore NaN's from 0/0
    })
  } else if (method=="vanilla") {
    vector("list",length(random.gov))
  }
   
  ### store metascore table back inside random.gov (~ 1.29 ms)
  random.gov <- lapply(X=1:length(random.gov),FUN=function(i){
    random.gov[[i]]$metascore <- metascores[[i]]
    random.gov[[i]]
  })
  return(random.gov)  
}

### --------------
### election routines
### --------------

vanillaWinner <- function(predictions) {
  set.seed(1234) # for reproducibility
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
  set.seed(1234) # for reproducibility
  
  ### random.gov[[i]]$committee's vote == committee.predictions[i]
  candidate.certainties <- sapply(X=seq.int(length(predictions)),
                            FUN=function(committee.num){
                              predicted.class <- predictions[committee.num]
                              random.gov[[committee.num]]$metascore[predicted.class]
                            })
  return(sample(predictions,size=1,prob=candidate.certainties))
}

processElections <- function(predictions.mat, random.gov, method="vanilla"){
  # no need to double check method, as this function is called from predict...
  out <- if (method=="vanilla") {
    apply(X=predictions.mat,MARGIN=2,FUN=vanillaWinner) 
  } else if (method=="synthetic") {
    apply(X=predictions.mat,MARGIN=2,FUN=function(committee.predictions,random.gov){syntheticMetascoreWinner(committee.predictions,random.gov)},random.gov)
  } else if (method=="prob") {
    apply(X=predictions.mat,MARGIN=2,FUN=function(committee.predictions,random.gov){probMetaScoreWinner(committee.predictions,random.gov)},random.gov)
  } else {
    stop("invalid method")
  }
  return(out)
}

### --------------
### prediction
### --------------

makeBinary <- function(data) {
  ### turns a vector/matrix into into a binary string/matrix
  ### via transform: ceiling( (data-min)/max )
  range.of.data <- range(data)
  ceiling( (data-range.of.data[1])/range.of.data[2] )
}

predictRandomGov <- function(random.gov,test.set,method="vanilla",dist="euclidean",parallel=FALSE) {
  ### check arguments in signature
  method <- match.arg(method, c("vanilla", "synthetic", "probabilistic"))
  dist <- match.arg(dist, c("euclidean","binary"))
  
  ### use kNN to block process test set and aggregate predictions
  predictCommittee <- function(committee, test.set, labels, dist) {
    training.set <- if (dist == "binary") {
      makeBinary(committee)
    } else {
      committee
    }
    knn(train=training.set,test=test.set,cl=labels,k=1)
  }
  
  if (dist == "binary") {test.set <- makeBinary(test.set)}
  
  predictions <- if (parallel) {
    foreach(committee.obj=iter(random.gov)) %dopar% {
      training.set <- committee.obj$committee
      labels <- committee.obj$labels
      predictCommittee(training.set, test.set, labels, dist) 
    }
      
  } else {
    lapply(X=random.gov,FUN=function(committee.obj, test.set, dist){
      predictCommittee(committee.obj$committee, test.set, committee.obj$labels, dist) 
      }, test.set, dist) 
  }
  
  ### process results into matrix
  predictions.mat <- matrix(unlist(predictions),nrow=length(random.gov),byrow=T)
  ### process elections
  return(processElections(predictions.mat,random.gov,method))
}

### --------------
### benchmarking
### --------------

estimatePerformance <- function(predicted.classes,actual.classes) {
  conf.mat <- table(predicted.classes,actual.classes)
  acc.est <- sum(diag(conf.mat))/length(actual.classes)
  return(list(confusionMatrix=conf.mat, accuracyEst=acc.est))
}

### ~ 9.65 s for 10 committees, 3.9k obs training set and 1k test set
buildTrainPredict <- function(building.set, training.set, training.set.response, test.set, test.set.response, build.type="generic", num.of.committees=10, class.width=1, method="vanilla", dist="euclidean", iter.limit=25, threshold=0.92,parallel=FALSE) {
  build.type <- match.arg(build.type, c("generic", "selective"))
  
  if (build.type == "generic") {
    random.gov <- buildRandomGov(building.set,num.of.committees,class.width)
    random.gov <- trainRandomGov(random.gov,training.set,training.set.response,method=method)
  } else if (build.type == "selective") {
    random.gov <- selectivelyBuildRandomGov(building.set, training.set, training.set.response, class.width, iter.limit, threshold, verbose=T)
  }
  predictions <- predictRandomGov(random.gov,test.set,method,dist,parallel)
  performance <- estimatePerformance(predictions,test.set.response)
  list(predictions=predictions, performance=performance, randomGov=random.gov)
}

### ----------
### cross validation routines (for vanilla prediction)
### ---------

### vanilla prediction doesn't require a training set.
### so, consider an arbitrary subset of digit.data

### load data
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}

assessVanillaPerformance <- function(num.of.committees, class.width, full.data.dt, test.set.size, parallel=FALSE) {
  
  pixel.cols <- colnames(full.data.dt)[which(!colnames(full.data.dt) %in% c("label"))]
  
  ### randomly divide whole data into "test" and "training" sets
  test.set.indices <- sample(seq.int(nrow(full.data.dt)), test.set.size, replace=F)
  test.set <- full.data.dt[test.set.indices]
  
  ### separate input and response
  test.set.response <- test.set[,label]
  test.set[,label:=NULL]
  
  ### make training set -- no need to separate input and response
  training.set <- full.data.dt[-test.set.indices]
  setkey(training.set,label)
  
  ### build random government on training set, then assess performance
  ### with both dist's on test set.
  
  random.gov <- buildRandomGov(training.set, pixel.cols, num.of.committees,class.width)
  
  `%op%` <- if (parallel) {`%dopar%`} else {`%do%`}
  
  preds <- foreach(distance=c("euc","binary")) %op% {
    predictRandomGov(random.gov,test.set,method="vanilla",dist=distance,parallel=F)
  }
  
  euc.preds <- preds[[1]]
  binary.preds <- preds[[2]]
  euc.performance <- estimatePerformance(euc.preds,test.set.response)
  binary.performance <- estimatePerformance(binary.preds,test.set.response)
  
  data.table(euclidean=euc.performance$accuracyEst,binary=binary.performance$accuracyEst)
}

crossValidateVanillaPerformance <- function(num.of.committees, class.width, full.data.dt, test.set.size,num.of.repeats=10,parallelize.folds=TRUE,parallelize.predictions=FALSE) {
  require(doRNG)
  require(data.table)
  `%op%` <- if (parallelize.folds) {`%dorng%`} else {`%do%`}
  set.seed(1234)
  performance.results <- foreach(i=seq.int(num.of.repeats),.final=rbindlist) %op% {
    assessVanillaPerformance(num.of.committees, class.width, full.data.dt, test.set.size, parallelize.predictions)
  }
  out <- cbind(num.of.committees=num.of.committees,class.width=class.width,performance.results[,lapply(.SD,mean)])
  data.table(out)
}


###-----
### note: for n committees, you need a building set of size at least 10*n.
### so that you can have 10 distinct classes per committee.
### You also want to have a test set of size >= 1k for decent 
### estimation of accuracy. If you want to train, you should also try and
### make a subset that has at least 3 samples from every class.
### Thus, make sure your subset is at easily bigger than 1k + 10*n + 30.
###----


# microbenchmark(out1 <- buildTrainPredict(building.set, training.set, training.set.response, test.set, test.set.response, build.type="g", num.of.committees=10, class.width=1, method="vanilla", dist="euclidean", iter.limit=3, threshold=0.8, parallel=TRUE), out2 <- buildTrainPredict(building.set, training.set, training.set.response, test.set, test.set.response, build.type="g", num.of.committees=10, class.width=1, method="vanilla", dist="euclidean", iter.limit=3, threshold=0.8, parallel=FALSE),times=5)

# 
# out <- selectiveBuildTrainPredict(building.set, training.set, training.set.response, test.set, test.set.response, class.width=1, iter.limit=50, threshold=0.92, method="prob") 
# 
# out2 <- selectiveBuildTrainPredict(building.set, training.set, training.set.response, test.set, test.set.response, class.width=1, iter.limit=50, threshold=0.92, method="vanilla") 
# 
# out3 <- selectiveBuildTrainPredict(building.set, training.set, training.set.response, test.set, test.set.response, class.width=3, iter.limit=15, threshold=0.92, method="prob") 
# 
# out4 <- selectiveBuildTrainPredict(building.set, training.set, training.set.response, test.set, test.set.response, class.width=45, iter.limit=1, threshold=0.92, method="vanilla") ; out4$performance

### accuracy is affected by posterior probabilities and class.width


# timing <- microbenchmark(out <- buildTrainPredict(num.of.committees,building.set,training.set,training.set.response,test.set,test.set.response,method="prob"),times=1)

done <- FALSE
if (done) {
  print(timing) # ~148s for 1k building set, 1k test, 8k train, 100 committees
  print(out$performance) # 46.1%
  metascore.dt <- data.table(matrix(unlist(lapply(X=out$randomGov,FUN=function(list){list$metascore})),byrow=T,ncol=10))
  best.committees.dt <- rbindlist(lapply(metascore.dt,function(col){data.table(max=max(col),row=which.max(col))}))
  
  pruned.random.gov <- out$randomGov[best.committees.dt[,row]]
  pruned.predictions <- predictRandomGov(pruned.random.gov,test.set,method=method)
  pruned.performance <- estimatePerformance(pruned.predictions,test.set.response) # 46.6%.
  
  
  out <- selectivelyBuildRandomGov(building.set,training.set,training.set.response,15,0.92,TRUE)
  
}

### sequential version that doesn't waste data:
# is.trained <- rep(FALSE,10)
# names(is.trained) <- as.factor(0:9)
# iter <- 0
# garbage <- vector("list",10)
# while (iter < 100) {
#   potential.committee <- building.set[,.SD[sample(x=.N, size=1)],by=label]
#   potential.committee.input <- potential.committee[,!"label",with=FALSE]
#   potential.committee.response <- potential.committee[,label]
#   predictions <- knn(train=potential.committee.input,
#                      test=training.set,
#                      cl=potential.committee.response,k=1)
#   counts <- table(predictions[predictions == training.set.response])
#   proportions <- counts / table(predictions)
#   
#   if (any( (proportions > 0.92) & !is.trained )) {
#       label.to.be.trained <- which.max((proportions > 0.92) & !is.trained)
#       garbage[[label.to.be.trained]] <- potential.committee
#       is.trained[label.to.be.trained] <- TRUE
#   }
#   iter <- iter+1
# }


# ### reference ~ 252.5 s
# system.time(ref.preds <- knn(train=training.set,test=test.set,cl=training.set.response,k=1))
# ### accuracy ~ 95%
# sum(diag(table(ref.preds,test.set.response)))/length(test.set.response)

### notes:
### it seems that training set size can quickly lead to over-fitting.
### I suspect there is a direct relationship between num.of.committees and
### accuracy when vanilla voting is implemented. Also, results seem very 
### dependent on distance used.