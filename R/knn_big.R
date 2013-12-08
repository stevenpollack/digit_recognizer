train<-read.csv("/Users/lucia/Documents/Berkeley/Fall_2013/240D/kag_digit_id/digit_id_data_and_benchmarks/train.csv")
test<-read.csv("/Users/lucia/Documents/Berkeley/Fall_2013/240D/kag_digit_id/digit_id_data_and_benchmarks/test.csv")

x.test<-test
x.learn<-train[,-1]
y.learn<-train[,1]

#Non-zero variance
colvars<-apply(x.learn, 2, var)
nonzero.learn<-x.learn[,colvars!=0]
nonzero.test<-x.test[,colvars!=0]

#F-test
fstats<-rep(NA, 784)
for (i in 1:784){
  est<-lm(x.learn[,i]~as.factor(y.learn))
  fstats[i]<-summary(est)$fstatistic[1]
}
keep<-order(fstats, na.last=F, decreasing=F)[584:784]
fstat.learn<-x.learn[,keep]
fstat.test<-x.test[,keep]

#Top 400 variances
keep<-order(colvars, decreasing=F)[385:784]
topvar.learn<-x.learn[,keep]
topvar.test<-x.test[,keep]

#Every other pixel
every2.learn<-x.learn[,seq(1, 784, by=2)]
every2.test<-x.test[,seq(1, 784, by=2)]

results.whole<-matrix(NA, nrow(x.test), 5)
results.fstat<-matrix(NA, nrow(x.test), 5)
results.nonzero<-matrix(NA, nrow(x.test), 5)
results.topvar<-matrix(NA, nrow(x.test), 5)
results.every2<-matrix(NA, nrow(x.test), 5)
votes.whole<-matrix(NA, nrow(x.test), 5)
votes.fstat<-matrix(NA, nrow(x.test), 5)
votes.nonzero<-matrix(NA, nrow(x.test), 5)
votes.topvar<-matrix(NA, nrow(x.test), 5)
votes.every2<-matrix(NA, nrow(x.test), 5)

for (K in 1:5){
  whole<-knn(x.learn,x.test,y.learn,K, prob=T)
  results.whole[,K]<-as.numeric(whole)-1
  votes.whole[,K]<-attributes(whole)$prob
  fstat<-knn(fstat.learn,fstat.test,y.learn,K, prob=T)
  results.fstat[,K]<-as.numeric(fstat)-1
  votes.fstat[,K]<-attributes(fstat)$prob
  nonzero<-knn(nonzero.learn,nonzero.test,y.learn,K,prob=T)
  results.nonzero[,K]<-as.numeric(nonzero)-1
  votes.nonzero[,K]<-attributes(nonzero)$prob
  topvar<-knn(topvar.learn,topvar.test,y.learn,K,prob=T)
  results.topvar[,K]<-as.numeric(topvar)-1
  votes.topvar[,K]<-attributes(topvar)$prob
  every2<-knn(every2.learn,every2.test,y.learn,K,prob=T)
  results.every2[,K]<-as.numeric(every2)-1
  votes.every2[,K]<-attributes(every2)$prob
}

results<-cbind(results.whole, results.nonzero, results.topvar, results.every2, results.fstat)

prop.correct<-function(x){
  diff<-results - y.test
  return(length(which(diff[,x]==0))/1000)
}
out<-apply(as.matrix(1:ncol(results)), 1, prop.correct)


prop<-cbind(out[1:5], out[6:10], out[11:15], out[16:20], out[21:25])


results.whole.cv<-matrix(NA, nrow(x.test), 5)
results.nonzero.cv<-matrix(NA, nrow(x.test), 5)
results.topvar.cv<-matrix(NA, nrow(x.test), 5)
results.every2.cv<-matrix(NA, nrow(x.test), 5)
results.fstat.cv<-matrix(NA, nrow(x.test), 5)
votes.whole.cv<-matrix(NA, nrow(x.test), 5)
votes.fstat.cv<-matrix(NA, nrow(x.test), 5)
votes.nonzero.cv<-matrix(NA, nrow(x.test), 5)
votes.topvar.cv<-matrix(NA, nrow(x.test), 5)
votes.every2.cv<-matrix(NA, nrow(x.test), 5)

for (K in 1:5){
  whole.cv<-knn.cv(x.learn, y.learn, K)
  results.whole.cv[,K]<-as.numeric(whole.cv)-1
  votes.whole.cv[,K]<-attributes(whole.cv)$prob
  fstat.cv<-knn.cv(fstat.learn, y.learn, K)
  results.fstat.cv[,K]<-as.numeric(fstat.cv)-1
  votes.fstat.cv[,K]<-attributes(fstat.cv)$prob
  nonzero.cv<-knn.cv(nonzero.learn, y.learn, K)
  results.nonzero.cv[,K]<-as.numeric(nonzero.cv)-1
  votes.nonzero.cv[,K]<-attributes(nonzero.cv)$prob
  topvar.cv<-knn.cv(topvar.learn, y.learn, K)
  results.topvar.cv[,K]<-as.numeric(topvar.cv)-1
  votes.topvar.cv[,K]<-attributes(topvar.cv)$prob
  every2.cv<-knn.cv(every2.learn, y.learn, K)
  results.every2.cv[,K]<-as.numeric(every2.cv)-1
  votes.every2.cv[,K]<-attributes(every2.cv)$prob
}

results.cv<-cbind(results.whole.cv, results.nonzero.cv, results.topvar.cv, results.every2.cv, results.fstat.cv)

prop.correct.cv<-function(x){
  diff<-results.cv - y.learn
  return(length(which(diff[,x]==0))/1000)
}
out.cv<-apply(as.matrix(1:ncol(results.cv)), 1, prop.correct.cv)
prop.cv<-cbind(out.cv[1:5], out.cv[6:10], out.cv[11:15], out.cv[16:20], out.cv[21:25])



##Binary
dist.mat<-dist(rbind(x.learn,x.test), method="binary")

results.whole.bin<- knn.binary(1:5, x.learn, y.learn, x.test, dist.mat)
results.fstat.bin<- knn.binary(1:5, fstat.learn, y.learn, fstat.test, dist.mat)
results.nonzero.bin<- knn.binary(1:5, nonzero.learn, y.learn, nonzero.test, dist.mat)
results.topvar.bin<- knn.binary(1:5, topvar.learn, y.learn, topvar.test, dist.mat)
results.every2.bin<- knn.binary(1:5, x.learn[,seq(1, 784, by=2)], y.learn, x.test[,seq(1, 784, by=2)], dist.mat)

results.bin<-cbind(results.whole.bin, results.nonzero.bin, results.topvar.bin, results.every2.bin, results.fstat.bin)

prop.correct.bin<-function(x){
  diff<-result.bin - y.test
  return(length(which(diff[,x]==0))/1000)
}
out.bin<-apply(as.matrix(1:25), 1, prop.correct)

prop.bin<-cbind(out.bin[1:5], out.bin[6:10], out.bin[11:15], out.bin[16:20], out.bin[21:25])




results.whole.bin.cv<- knn.binary.cv(1:5, x.learn, y.learn, dist.mat)
results.fstat.bin.cv<- knn.binary.cv(1:5, fstat.learn, y.learn, dist.mat)
results.nonzero.bin.cv<- knn.binary.cv(1:5, nonzero.learn, y.learn, dist.mat)
results.topvar.bin.cv<- knn.binary.cv(1:5, topvar.learn, y.learn, dist.mat)
results.every2.bin.cv<- knn.binary.cv(1:5, x.learn[,seq(1, 784, by=2)], y.learn, dist.mat)

results.bin.cv<-cbind(results.whole.bin.cv, results.nonzero.bin.cv, results.topvar.bin.cv, results.every2.bin.cv, results.fstat.bin.cv)

prop.correct.bin.cv<-function(x){
  diff<-results.bin.cv - y.learn
  return(length(which(diff[,x]==0))/1000)
}
out.bin.cv<-apply(as.matrix(1:25), 1, prop.correct.bin.cv)

prop.bin.cv<-cbind(out.bin.cv[1:5], out.bin.cv[6:10], out.bin.cv[11:15], out.bin.cv[16:20], out.bin.cv[21:25])





