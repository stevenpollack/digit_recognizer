### check that data is loaded
if (!"digit.data" %in% ls()) {
  source(file="R/load_data.R")
}

### check that unlabeled data is load 
if (!"unlabeled.digit.data" %in% ls()) {
  load(file="data_and_benchmarks/testing_data.Rdata")
}

library(kernlab) # for ksvm()

### load grizzlybear results for cross-validated
### full-data SVM's
load("grizzlybear_results.Rdata")

best.model <- ksvm(label~., data=digit.data, C=best.cost)

### save intermediary work
save(best.model, file="optimal_full-data_svm.Rdata")

### predict test set values and save as csv
test.predictions <- predict(best.model,unlabeled.digit.data)

write.csv(data.frame(ImageId=1:28000,
Label=test.predictions),file="cv-cost_svm_submission.csv",row.names=F,quote=F)
