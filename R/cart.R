library(rpart)
library(rpart.plot)

prediction<-apply(as.matrix(seq(0,0.1, by=.005)),1,FUN=function(x){
  fit<-rpart(label~., learn, method="class", control=rpart.control(cp=x))
  y.fit<-predict(fit, test, type="class")
  return(as.numeric(y.fit)-1)
})

prediction.learn<-apply(as.matrix(seq(0,0.1, by=.005)),1,FUN=function(x){
  fit<-rpart(label~., learn, method="class", control=rpart.control(cp=x))
  y.fit<-predict(fit, learn, type="class")
  return(as.numeric(y.fit)-1)
})

diff<-prediction - y.test
diff.learn<-prediction.learn - y.learn
prop.acc<-apply(as.matrix(1:dim(prediction)[2]), 1, FUN = function(x){
  length(which(diff[,x]==0))/1000})
prop.acc.learn<-apply(as.matrix(1:dim(prediction.learn)[2]), 1, FUN = function(x){
  length(which(diff.learn[,x]==0))/1000})
cbind(seq(0,0.1, by=.005), prop.acc, prop.acc.learn)

table(prediction[,1], y.test)


plot(seq(0,.1,.005), prop.acc.learn, type="l", col="blue", xlab="cp", ylab="Proportion Accuracy",
     main="Proportion Accuracy vs. CP")
lines(seq(0,.1,.005), prop.acc, col="red")
legend("bottomleft", "(x,y)", c("Within Train", "Test"), lty=1, col=c("blue", "red"))

prediction.min<-apply(as.matrix(seq(20,300, by=20)),1,FUN=function(x){
  fit<-rpart(label~., learn, method="class", control=rpart.control(minsplit=x))
  y.fit<-predict(fit, test, type="class")
  return(as.numeric(y.fit)-1)
})

prediction.min.learn<-apply(as.matrix(seq(20,300, by=20)),1,FUN=function(x){
  fit<-rpart(label~., learn, method="class", control=rpart.control(minsplit=x))
  y.fit<-predict(fit, learn, type="class")
  return(as.numeric(y.fit)-1)
})


diff.min<-prediction.min - y.test
diff.min.learn<-prediction.min.learn - y.learn
prop.acc.min<-apply(as.matrix(1:dim(prediction.min)[2]), 1, FUN = function(x){
  length(which(diff.min[,x]==0))/1000})
prop.acc.learn.min<-apply(as.matrix(1:dim(prediction.min.learn)[2]), 1, FUN = function(x){
  length(which(diff.min.learn[,x]==0))/1000})
cbind(seq(20,300,by=20),prop.acc.min, prop.acc.learn.min)

plot(seq(20,300,20), prop.acc.learn.min, type="l", col="blue", xlab="Minimum Observations in Node for Split", ylab="Proportion Accuracy",
     main="Proportion Accuracy vs. Minimum Obs in Node")
lines(seq(20,300,20), prop.acc.min, col="red")
legend("bottomleft", "(x,y)", c("Train Accuracy", "Test Accuracy"), lty=c(1,1), col=c("blue", "red"))

#Variable Importance

var.import<-apply(as.matrix(seq(0,0.1, by=.005)),1,FUN=function(x){
  rpart(label~., learn, method="class", control=rpart.control(cp=x))$variable.importance[1:10]
})

var.import.min<-apply(as.matrix(seq(20,300, by=20)),1,FUN=function(x){
  names(rpart(label~., learn, method="class", control=rpart.control(minsplit=x))$variable.importance[1:10])
})

output<-unlist(var.import)
names(output)
out.put<-matrix(names(output), ncol=18)
table(out.put)[order(table(out.put))]

output.min<-unlist(var.import.min)

for.fun<-table(output.min)[order(table(output.min))]
tab<-for.fun[7:length(for.fun)]

subset.x<-x.learn[,names(tab)]
data.prac<-cbind(y.learn, subset.x)
fit<-rpart(y.learn~., data.prac, method="class")
y.pred.fit<-predict(fit, x.test[,names(tab)], type="class")

diff.fit<-as.numeric(prediction)-1 - y.test
length(which(diff.fit==0))/1000
prop.acc<-apply(as.matrix(1:dim(prediction)[2]), 1, FUN = function(x){
  length(which(diff[,x]==0))/1000})

#Splits
base$splits[base$splits[,1]!=0,]