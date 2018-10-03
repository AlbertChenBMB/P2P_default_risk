#undersample

library(unbalanced)
input<-dataset2016[,-dataset2016$loan_status]# have to change to number
Y<-dataset2016$loan_status
data<-ubUnder(X=input,Y= Y,perc = 50,method="percPos")
newData<-cbind(data$X,data$Y)
