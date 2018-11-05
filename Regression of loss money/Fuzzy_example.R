#frbs example 
#reference :https://www.jstatsoft.org/article/view/v065i06/v65i06.pdf P14~19
install.packages("frbs")
library(frbs)
data("iris",package = "datasets")
irisShuffled<-iris[sample(nrow(iris)),]
#change label to numerice
irisShuffled[,5]<-unclass(irisShuffled[,5])
#find the range of each features
range.data.input<-apply(iris[,-ncol(iris)],2,range)
#train set and test set
tra.iris<-irisShuffled[1:140,]
tst.iris<-irisShuffled[141:nrow(irisShuffled),1:4]
real.iris<-matrix(irisShuffled[141:nrow(irisShuffled),5],ncol=1)
#Model generation
#method.type we should change to ANFIS
object.frbs.w<-frbs.learn(tra.iris,range.data = range.data.input,
                          method.type = "FRBCS.W",control=list(num.labels=3,type.mf="TRAPEZOID"))
summary(object.frbs.w)
#prediction with new data
pred<-predict(object.frbs.w,tst.iris)
err<-100*sum(pred!=real.iris)/length(pred)
err
