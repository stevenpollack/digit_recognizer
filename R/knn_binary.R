dist.mat<-dist(rbind(x.learn,x.test), method="binary")

knn.binary <- function(klist,x.train,y.train,x.test, trial) {
  # k-nearest neighbors classification
  # 
  # klist is a list of values of k to be tested
  # x.train, y.train: the training set
  # x.test: the test set
  # Output: a matrix of predictions for the test set (one column for each k in klist)	
  # Number of training and test examples
  n.train <- nrow(x.train)
  n.test <- nrow(x.test)
  
  # Matrix to store predictions
  p.test <- matrix(NA, n.test, length(klist))
  
  # Vector to store the distances of a point to the training points
  #trial <- dist(rbind(x.train, x.test), method="binary")
  dbin<-as.matrix(trial)[1001:2000,1:1000]
  
  # Loop on the test instances
  for (tst in 1:n.test)
  {
    # Compute distances to training instances
#     for (trn in 1:n.train)
#     {
#       dsq[trn] <- sum((x.train[trn,] - x.test[tst,])^2)
#     }
    
    # Sort distances from smallest to largest
    ord <- order(dbin[tst,])
    
    p.test[tst,1]<-y.train[ord[1]]
    
    # Make prediction by averaging the k nearest neighbors
    for (ik in 2:length(klist)) {
      classes<-table(as.factor(y.train)[ord[1:ik]])
      m<-which(classes==max(classes))
      if(length(m)!=1){m = m[sample.int(length(m), 1)]}
      else m=m
      p.test[tst,ik]<-as.numeric(names(classes)[m])
    }
  }
  
  # Return the matrix of predictions
  return(p.test)
}


results.whole.bin<- knn.binary(1:10, x.learn, y.learn, x.test, dist.mat)
results.nonzero.bin<- knn.binary(1:10, nonzero.learn, y.learn, nonzero.test, dist.mat)
results.topvar.bin<- knn.binary(1:10, topvar.learn, y.learn, topvar.test, dist.mat)
results.topvar50.bin<- knn.binary(1:10, topvar50.learn, y.learn, topvar50.test, dist.mat)
results.every2.bin<- knn.binary(1:10, x.learn[,seq(1, 784, by=2)], y.learn, x.test[,seq(1, 784, by=2)], dist.mat)

results.bin<-cbind(results.whole.bin, results.nonzero.bin, results.topvar.bin, results.topvar50.bin, results.every2.bin)

prop.correct.bin<-function(x){
  diff<-result.bin - y.test
  return(length(which(diff[,x]==0))/1000)
}
out.bin<-apply(as.matrix(1:50), 1, prop.correct)

prop.bin<-cbind(out.bin[1:10], out.bin[11:20], out.bin[21:30], out.bin[31:40], out.bin[41:50])

knn.binary.cv <- function(klist,x.train,y.train, trial) {
  # k-nearest neighbors classification
  # 
  # klist is a list of values of k to be tested
  # x.train, y.train: the training set
  # x.test: the test set
  # Output: a matrix of predictions for the test set (one column for each k in klist)  
  # Number of training and test examples
  n.train <- nrow(x.train)
  
  # Matrix to store predictions
  p.test <- matrix(NA, n.train, length(klist))
  
  # Vector to store the distances of a point to the training points
  #trial <- dist(rbind(x.train, x.test), method="binary")
  dbin<-as.matrix(trial)[1:1000,1:1000]
  
  # Loop on the test instances and cv
  for (tst in 1:n.train)
  {
    # Sort distances from smallest to largest
    distances<-dbin[tst,]
    distances[tst]<-NA
    ord <- order(distances)
    
    p.test[tst,1]<-y.train[ord[1]]
    
    # Make prediction by averaging the k nearest neighbors
    for (ik in 2:length(klist)) {
      classes<-table(as.factor(y.train)[ord[1:ik]])
      m<-which(classes==max(classes))
      if(length(m)!=1){m = m[sample.int(length(m), 1)]}
      else m=m
      p.test[tst,ik]<-as.numeric(names(classes)[m])
    }
  }
  
  # Return the matrix of predictions
  return(p.test)
}

results.whole.bin.cv<- knn.binary.cv(1:10, x.learn, y.learn, dist.mat)
results.nonzero.bin.cv<- knn.binary.cv(1:10, nonzero.learn, y.learn, dist.mat)
results.topvar.bin.cv<- knn.binary.cv(1:10, topvar.learn, y.learn, dist.mat)
results.topvar50.bin.cv<- knn.binary.cv(1:10, topvar50.learn, y.learn, dist.mat)
results.every2.bin.cv<- knn.binary.cv(1:10, x.learn[,seq(1, 784, by=2)], y.learn, dist.mat)

results.bin.cv<-cbind(results.whole.bin.cv, results.nonzero.bin.cv, results.topvar.bin.cv, results.topvar50.bin.cv, results.every2.bin.cv)

prop.correct.bin.cv<-function(x){
  diff<-results.bin.cv - y.test
  return(length(which(diff[,x]==0))/1000)
}
out.bin.cv<-apply(as.matrix(1:50), 1, prop.correct.bin.cv)

prop.bin.cv<-cbind(out.bin.cv[1:10], out.bin.cv[11:20], out.bin.cv[21:30], out.bin.cv[31:40], out.bin.cv[41:50])


