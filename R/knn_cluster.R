#K-NN for cluster

library(class)

train<-read.csv("/Users/lucia/Documents/Berkeley/Fall_2013/240D/kag_digit_id/digit_id_data_and_benchmarks/train.csv")
test<-read.csv("/Users/lucia/Documents/Berkeley/Fall_2013/240D/kag_digit_id/digit_id_data_and_benchmarks/test.csv")

x.test<-test
x.learn<-train[,-1]
y.learn<-train[,1]

out1<-knn(x.learn, x.test[1:10000,], y.learn)
save(out1,file="knn1.Rdata")

out2<-knn(x.learn, x.test[10001:20000,],y.learn)
save(out2,file="knn2.Rdata")

out3<-knn(x.learn, x.test[20001:28000,], y.learn)
save(out3,file="knn3.Rdata")