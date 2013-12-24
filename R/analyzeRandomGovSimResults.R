### analyzeRandomGovSimResults.R
library(data.table)
library(ggplot2)
library(reshape2)

load("mpi_cv_vanilla_random_gov_17h55--100_class_widths.Rdata")
### results generated from script run in mpi_cv_rg_as_knn.Rout

knn.simulation <- par.grid.results

head(melt(data=knn.simulation,measure.vars=c("euclidean","binary","elapsed")))
