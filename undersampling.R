#undersample

library(unbalanced)
input<-dataset2016[,-dataset2016$statue]# have to change to number
Y<-dataset2016$statue
data<-ubUnder(X=input,Y= Y,perc = 40,method="percPos")
newData<-cbin(data$)