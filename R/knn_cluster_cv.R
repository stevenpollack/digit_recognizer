library(class)

train<-read.csv("/Users/lucia/Documents/Berkeley/Fall_2013/240D/kag_digit_id/digit_id_data_and_benchmarks/train.csv")

x.learn<-train[,-1]
y.learn<-train[,1]

out1.cv<-knn(x.learn, y.learn, k=1, prob=T)
save(out1.cv,file="knn1_cv.Rdata")

out3.cv<-knn(x.learn, y.learn, k=3, prob=T)
save(out3.cv,file="knn3_cv.Rdata")

out5.cv<-knn(x.learn, y.learn, k=5, prob=T)
save(out5.cv,file="knn5_cv.Rdata")

#Non-zero variance
colvars<-apply(x.learn, 2, var)
nonzero.learn<-x.learn[,colvars!=0]

out1.cv.nz<-knn(nonzero.learn, y.learn, k=1, prob=T)
save(out1_nz.cv,file="knn1_cv.Rdata")

out3.cv.nz<-knn(nonzero.learn, y.learn, k=3, prob=T)
save(out3_nz.cv,file="knn3_cv.Rdata")

out5.cv.nz<-knn(nonzero.learn, y.learn, k=5, prob=T)
save(out5_nz.cv,file="knn5_cv.Rdata")