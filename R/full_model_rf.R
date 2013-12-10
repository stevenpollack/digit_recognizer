### optimal parameters are ntree=500, mtry=56 over 35,000 training examples
library(doRNG)
library(randomForest)
library(doParallel)
registerDoParallel(cores=detectCores())

multiRF <- function(X, Y, mtry_vector,...) {
  stopifnot(is.numeric(mtry_vector))
  stopifnot(all(mtry_vector>1))
  
  foreach(i=mtry_vector,.combine=randomForest::combine,.packages='randomForest',.export=c('X','Y'),.inorder=FALSE) %dorng% {
    randomForest(X,Y,mtry=i,...)
  }
}

### check that data is loaded
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}

set.seed(1234)
training.indices <- sample(1:42000,size=35000)
training.set.x <- digit.data[training.indices,-1]
training.set.y <- digit.data[training.indices,1]

test.set.x <- digit.data[-training.indices,-1]
test.set.y <- digit.data[-training.indices,1]

mtry_vector <- rep(x=56,times=16)
ntree <- 62

par.time <- system.time(par.rf <- multiRF(X=training.set.x, Y=training.set.y, mtry_vector=mtry_vector, ntree=ntree))

file.name <- paste("cv_rf-results-", format(Sys.time(), "%H:%M:%S-%d-%m-%Y"),".Rdata",sep="")
save(training.indices, par.time, par.rf, file=file.name)

### estimate accuracy from remainder of data set
test.preds <- predict(par.rf,test.set.x)
confusionMatrix <- table(test.preds,test.set.y)
accuracy.est <- sum(diag(confusionMatrix))/length(test.preds)

### predict unlabeled data
load(file="data_and_benchmarks/testing_data.Rdata")

### predict test set values and save as csv
competition.preds <- predict(par.rf,unlabeled.digit.data)

write.csv(data.frame(ImageId=1:28000,Label=competition.preds),
          file="cv-rf_submission.csv",row.names=F,quote=F)

save(training.indices, par.time, par.rf, confusionMatrix, accuracy.est, file=file.name)
