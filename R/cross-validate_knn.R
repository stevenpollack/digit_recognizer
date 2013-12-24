### cross validates kNN using doParallel or doMPI
### depending on the cluster that script is run on.

### -------
### Preliminary work
### -------

### load data
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}

num.of.folds <- 10 # parameter to be changed, maybe later

processDTList <- function(dt.list) { # clean up function for inner-loop
  combined.dt <- rbindlist(dt.list)
  out <- combined.dt[,!"test.fold",with=F][,lapply(.SD,mean)]
  setnames(out,old=c(2,3),new=c("avg.accuracy","avg.test.time") )
}

subset.sizes <- seq(from=1000,to=nrow(digit.data),by=500)

### set up reproducible RNG
library(doRNG)
rng <- RNGseq(length(subset.sizes) * num.of.folds, 1234)

### -------
### cross validation code
### ------

source("R/determineParallelBackend.R") # returns flag useMPI for cleanup routine

cv.results <- foreach(i=seq.int(length(subset.sizes)),
                      .final=rbindlist,
                      .packages=c("class","data.table","doRNG")
) %:%
  foreach(test.fold=seq.int(num.of.folds),
          seeds=rng[(i - 1) * num.of.folds + 1 : num.of.folds],
          .inorder=FALSE,
          .final=processDTList) %dopar%
{         
  subset.size=subset.sizes[i]
  stopifnot(subset.size %% num.of.folds == 0)
  fold.size <- subset.size/num.of.folds
  
  # set RNG seed
  rngtools::setRNG(seeds)
  
  subset.indices <- sample(x=nrow(digit.data),size=subset.size,replace=FALSE)
  test.set.indices <- sample(x=subset.size,size=fold.size,replace=FALSE)
  
  subset.dt <- digit.data[subset.indices]
  subset.dt[sample.int(subset.size), fold.num := unlist(lapply(seq.int(num.of.folds),rep.int,times=num.of.folds)) ]
  setkey(subset.dt,fold.num)
  
  pixel.cols <- colnames(subset.dt)[which(!colnames(subset.dt) %in% c("label","fold.num"))]
  
  train.time <- system.time(predictions <- knn(train=subset.dt[!J(test.fold),pixel.cols,with=FALSE],test=subset.dt[J(test.fold),pixel.cols,with=FALSE],cl=subset.dt[!J(test.fold),label],k=test.fold))
  test.accuracy <- mean(predictions == subset.dt[J(test.fold),label][,label])
  
  out <- data.table(subset.size=subset.size,test.fold=test.fold,fold.accuracy=test.accuracy,train.time=train.time[3])
  
  out  
}

save(cv.results,file="parallel_10-fold_knn_acc_and_timing_benchmark-00h05.Rdata")

### -------
### clean up / close cluster
### ------

if (useMPI) {
  cat("closing cluster and finalizing MPI...\n")
  closeCluster(cl)
  mpi.finalize()
}