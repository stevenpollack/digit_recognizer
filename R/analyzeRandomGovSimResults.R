### analyzeRandomGovSimResults.R
library(data.table)
library(ggplot2)
#library(reshape2) # melting a data.table crashes R.

load("mpi_cv_vanilla_random_gov_17h55--100_class_widths.Rdata")
### results generated from script run in mpi_cv_rg_as_knn.Rout

knn.simulation <- data.frame(par.grid.results)

ggplot(data=knn.simulation,aes(x=class.width)) + geom_line(aes(y=binary)) + geom_point(aes(y=binary)) + geom_line(aes(y=euclidean),color='red') + geom_point(aes(y=euclidean),color='red')

ggplot(data=knn.simulation,aes(x=class.width)) + geom_line(aes(y=binary))  + geom_line(aes(y=euclidean),color='red') 

ggplot(data=knn.simulation,aes(x=class.width)) + stat_smooth(aes(y=binary),color='black') + stat_smooth(aes(y=euclidean),color='red')

ggplot(data=knn.simulation,aes(x=class.width)) + stat_smooth(aes(y=elapsed)) + geom_point(aes(y=elapsed),alpha=0.5,color='red') + geom_abline(intercept=6,slope=0.35,color='green',lty=2) # training time seems to grow linearly with class.width

head(melt(data=knn.simulation,measure.vars=c("euclidean","binary","elapsed")))
