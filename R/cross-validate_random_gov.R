source("R/knn_based_random_gov.R")
source("R/determineParallelBackend.R") # returns flag useMPI for cleanup routine

### -------
### cross-validation
### ------

par.grid <- as.matrix(expand.grid(num.of.committees=seq(from=3,to=51,by=2),class.width=seq.int(100)))

num.of.repeats <- 10

par.grid.results <- foreach(parameters=iter(par.grid,by="row"),.final=rbindlist) %do% {
  num.of.committees <- parameters[1]
  class.width <- parameters[2]
  cat("cross validating accuracy for class.width =", class.width, " and", num.of.committees, "num.of.committees...\n")
  cv.time <- system.time(
    results <- crossValidateVanillaPerformance(
      num.of.committees=num.of.committees,
      class.width=class.width,
      full.data.dt=digit.data,
      test.set.size=1000,
      num.of.repeats=num.of.repeats,
      parallelize.folds=TRUE,
      parallelize.predictions=FALSE
    )
  )
  
  out <- data.table(cbind(results,user=cv.time[1],elapsed=cv.time[3]))
  cat("**********************************\n")
  print(out)
  cat("**********************************\n")
  return(out)
}

save(par.grid.results, file="mpi_cv_class_widths-and-_num_committees_vanilla_random_gov_18h15.Rdata")

### -------
### clean up / close cluster
### ------

if (useMPI) {
  cat("closing cluster and finalizing MPI...\n")
  closeCluster(cl)
  mpi.finalize()
}
