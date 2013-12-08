### load kaggle competition data
unlabeled.digit.data <- read.csv(file="data_and_benchmarks/test.csv",header=T)
save(unlabeled.digit.data,file="data_and_benchmarks/testing_data.Rdata")

### ^^ this will be exported to another script later

library(kernlab)
### test out full-data SVM
load("cross-validated_svm_results-02:03:26-08-12-2013.Rdata")

full.data.svm <- model.seq[[1]]$svm
preds <- predict(full.data.svm,unlabeled.digit.data)

write.csv(data.frame(ImageId=1:28000, Label=preds),file="c1_svm_submission.csv",row.names=F,quote=F)