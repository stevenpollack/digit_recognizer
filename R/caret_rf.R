### grow a forest on Lucia's test and learning sets
load("data_and_benchmarks/learn_test.RData")

learn[,1] <- as.factor(learn[,1])

library(caret)
library(randomForest)
library(doMC)
registerDoMC(cores=detectCores())
par.grid <- createGrid(method="parRF",len=19,data=learn)
ctrl <- trainControl(method="cv",number=10,repeats=10,verboseIter=T)
train.time <- system.time( rf.caret <- train(label~.,data=learn,
                                             method="parRF",
                                             tuneGrid=par.grid,
                                             trControl=ctrl,
                                             ntree=250,
                                             importance=T) )

workspace.vars <- ls()[which(!ls() %in% c("digit.data","training.set", "learn", "test"))]
save(list=workspace.vars, file=paste("lucia_rf-", format(Sys.time(), "%H:%M:%S-%d-%m-%Y"),".Rdata",sep=""))