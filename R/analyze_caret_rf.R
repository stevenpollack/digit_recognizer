### analyze caret_rf.R
load("data_and_benchmarks/learn_test.RData")
load("lucia_rf-2.Rdata")

test[,1] <- as.factor(test[,1])

library(caret)
library(randomForest)
library(data.table)

### predict against test
best.rf <- lucia_rf
predicted.values <- predict(best.rf,test)
confusionMat <- table(predicted.values, test[,1])
accuracy.est <- sum(diag(confusionMat))/1000 # 90.2%

### where did it fail?
test <- data.table(test)
setkey(test,label)
class.count <- test[,length(pixel0),by=label][,V1]
confusionMat <- cbind(confusionMat,"class count"=class.count)

var.importance <- data.table(Variable=rownames(best.rf$importance),best.rf$importance[,11:12])
setkey(var.importance,MeanDecreaseGini)

top.40.mdg <- var.importance[745:784,Variable]

library(ggplot2)

mdg.plot <- ggplot() + geom_point(aes(x=var.importance[MeanDecreaseGini>3.75,ordered(Variable,levels=Variable)],y= var.importance[MeanDecreaseGini>3.75,MeanDecreaseGini])) + coord_flip() + labs(x="pixel",y="Mean Decrease in Gini", title=expression(paste("Pixels with MDG > ",  3.75)))

setkey(var.importance,MeanDecreaseAccuracy)
top.40.mda <- var.importance[745:784,Variable]

mda.plot <- ggplot() + geom_point(aes(x=var.importance[MeanDecreaseAccuracy>0.005,ordered(Variable,levels=Variable)],y= var.importance[MeanDecreaseAccuracy>0.005,MeanDecreaseAccuracy])) + coord_flip() + labs(x="pixel",y="Mean Decrease in Accuracy", title=expression(paste("Pixels with MDA > ",  0.005)))

### find top 40 vars with MDG and MDA
top.40.mdg.and.mda <- intersect(top.40.mda,top.40.mdg)