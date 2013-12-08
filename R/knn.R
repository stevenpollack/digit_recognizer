library(class)
load("/Users/lucia/Documents/Berkeley/Fall_2013/240D/kag_digit_id/R/knn.RData")
orig.train<-read.csv("/Users/lucia/Documents/Berkeley/Fall_2013/240D/kag_digit_id/digit_id_data_and_benchmarks/train.csv")
orig.test<-read.csv("/Users/lucia/Documents/Berkeley/Fall_2013/240D/kag_digit_id/digit_id_data_and_benchmarks/test.csv")

#get data
set.seed(123123)
#train <- read.csv("hw3train.csv")
learning.ids<-sample.int(32000, size=1000, replace=F)
testing.ids<-sample.int(32000, size=1000, replace=F)
learn <- train[learning.ids,]
test <- train[testing.ids,]
#keep pixels that have non-zero variance
colvars<-apply(learn, 2, var)
cl.learn<-learn[,1]
cl.test<-test[,1]
learn<-learn[,-1]
test<-test[,-1]
nonzero <- learn[,colvars[-1]!=0]

prac.learn<-learn[,colnames(nonzero)]
prac.test<-test[,colnames(nonzero)]

#Euclidean distance, vary values of k
knn.func<-function(K){
  pred<-knn(prac.learn, prac.test, cl.learn, k=K, prob=T)
  diff<-(as.numeric(pred)-1)-cl.test
#It predicts on a 1-10 scale, we need classes to be 0-9
  return(list(c(length(which(diff==0))/1000), as.numeric(pred)-1, attributes(pred)$prob))
}
out<-lapply(as.matrix(1:10), knn.func)
prop<-rep(NA, 10)
for(i in 1:10){
  prop[i]<-out[[i]][1]
}

knn.cv.func<-function(K){
  fold<-rep(1:10, each=100)
  prediction<-matrix(NA, nrow=1000, ncol=10)
  #votes<-matrix(NA, nrow=1000, ncol=10)
  for (i in 1:10){
    pred<-knn(prac.learn[fold!=i,], prac.test, cl.learn[fold!=i], k=K, prob=T)
    prediction[,i]<-as.numeric(pred)-1
    #votes[,i]<-attributes(pred)$prob
  }
  pred<-apply(prediction, 1, mode)
  return(c(pred)) #, votes)))
}
out1<-apply(as.matrix(1:10), 1, knn.cv.func)

out<-knn(train[,-1], test, train[,1], k=3)

#{Nearest neighbor classifiers.} Examine the performance of \textit{k-nearest neighbor classifiers} 
#($k$-NN) for varying values of the number of neighbors $k$, varying sets of pixels, and both the Euclidean and 
#binary distance functions (\verb|dist| function).  Report and comment on learning and test set error rates and 
#vote proportions.

#Vary number of neighbors $k$


