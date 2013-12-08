library(plyr)

extractCVError <- function(model.seq) {
  ### probably could be optimized with ldply...
  out <- llply(.data=model.seq, .fun=function(list.obj){
    if (!is.null(names(list.obj))){
      list.obj$svm@cross
    } else {
      NA
    }})
  return(unlist(out))
}

### second batch
load("cross-validated_svm_results-04:34:59-08-12-2013.Rdata")
cv.seq2 <- extractCVError(model.seq)
rm("cost.seq"); rm("model.seq")
save.image(file="grizzlybear_results.Rdata")

### first batch
load("cross-validated_svm_results-04:40:39-08-12-2013.Rdata")
cv.seq1 <- extractCVError(model.seq)
rm("cost.seq"); rm("model.seq")
save.image(file="grizzlybear_results.Rdata")

### third batch
load("cross-validated_svm_results-04:49:39-08-12-2013.Rdata")
cv.seq3 <- extractCVError(model.seq)
rm("cost.seq"); rm("model.seq")

cv.seq <- c(cv.seq1, cv.seq2, cv.seq3)
cost.seq <- seq(from=1,to=100,length.out=25)

grizzlybear.results.df <- data.frame(cost=cost.seq[1:24], cv.error=cv.seq)
best.cost <- grizzlybear.results.df[which.min(cv.seq),1]

save(grizzlybear.results.df,best.cost, file="grizzlybear_results.Rdata")