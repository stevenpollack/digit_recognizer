source("R/knn_based_random_gov.R")

par.grid <- expand.grid(num.of.committees=seq(from=3,to=15,by=2),class.width=3:25)

num.of.repeats <- 10

par.grid.results <- apply(X=par.grid, MARGIN=1, FUN=function(parameters) {
  num.of.committees <- parameters[1]
  class.width <- parameters[2]
  cv.time <- system.time(results <- crossValidateVanillaPerformance(num.of.committees,class.width,digit.data,1000,num.of.repeats))
  
  return(data.table(cbind(results,user=cv.time[1],elapsed=cv.time[3])))
})

par.grid.results <- rbindlist(par.grid.results)

save(par.grid.results, file="cv_vanilla_random_gov_23h23.Rdata")
