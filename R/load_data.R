### clear enviroment
rm(list=ls())

### loads and "conditions" data for classification
digit.data <- read.csv(file="data_and_benchmarks/train.csv",header=T)
rownames(digit.data) <- paste("image",1:42000,sep="")
digit.data[,1] <- as.factor(digit.data[,1])

### save digit.data into .Rdata file
save.image(file="data_and_benchmarks/training_data.Rdata")