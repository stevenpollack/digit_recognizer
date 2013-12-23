### determineParallelBackend.R
###
### Script runs through logic to determine whether doParallel or doMPI should
### be used to retister a backend.
###
### Will fail if either doParallel or doMPI are not installed.
###
### Be sure to end whatever to-be-parallelized code with:
###
### if (useMPI) {
###   cat("closing cluster and finalizing MPI...\n")
### 	closeCluster(cl)
### 	mpi.finalize()
### }

nHosts <- as.numeric(Sys.getenv('NHOSTS'))
nCores <- as.numeric(Sys.getenv('NSLOTS'))

if (is.na(nHosts) || nHosts == 1) { # running locally or on a single host
  useMPI <- FALSE
  require(doParallel)
  
  # check to see if running locally, so you can grab all cores
  if (is.na(nCores)) nCores <- detectCores() 
  
  registerDoParallel(cores=nCores)
  cat("running script inside a single node (", nCores, "CPUs) via doParallel...\n")
} else {
  useMPI <- TRUE
  cat("running script across", nHosts, "nodes with", nCores, "cores via MPI...\n")
  
  require(doMPI)
  cl <- startMPIcluster()
  registerDoMPI(cl)
  exportDoMPI(cl,varlist=ls())
}