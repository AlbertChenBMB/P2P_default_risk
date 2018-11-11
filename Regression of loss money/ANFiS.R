######################
#fbs.test
library(frbs)

test.dataset<-tra.dataset[sample(nrow(tra.dataset)),]
test.dataset<-select(test.dataset,c(1,2,3,4,6,7,8,9,
                                    11,12,13,14,15,16,
                                    17,18,19,20,21,22,
                                    23,24,25,26,27,28,29,30,31,32,
                                    34,35,38,40,43,42))
#find the range of each features
range.data.input<-apply(test.dataset[,-ncol(test.dataset)],2,range)
#train set and test set
tra<-test.dataset[1:183000,]
tst<-test.dataset[183001:nrow(test.dataset),1:35]
real<-matrix(test.dataset[183001:nrow(test.dataset),36],ncol=1)
#Model generation
#method.type we should change to ANFIS
object.frbs.w<-frbs.learn(tra,range.data = range.data.input,
                          method.type = "ANFIS")
summary(object.frbs.w)

