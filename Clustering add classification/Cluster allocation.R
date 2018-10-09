#testdata spired to different cluster
# get the center of eatch cluster
center<-kmeans$centers
list1<-as.matrix(center[1,])
list2<-as.matrix(center[2,])
list3<-as.matrix(center[3,])
list4<-as.matrix(center[4,])
list5<-as.matrix(center[5,])

#scale testdata and select features
newt<-testset0[c(1,2,3,4,7,9,11,13,28,31,15)]
Y<-testset0[c(1,2,3,4,7,9,11,13,28,31)]
Y<-scale(Y)
# chose features without label 
newt[1:10]<-scale(newt[1:10])
tbl_df(Y)

#allocate of data into different cluster
testcluster<-list()
for(i in 1:length(Y[,1])){
  distance1<-0
  distance2<-0
  distance3<-0
  distance4<-0
  distance5<-0
  for (j in 1:10){
    distance1<- distance1+((list1[j]-Y[i,j])**2)
    distance2<- distance2+((list2[j]-Y[i,j])**2)
    distance3<- distance3+((list3[j]-Y[i,j])**2)
    distance4<- distance4+((list4[j]-Y[i,j])**2)
    distance5<- distance5+((list5[j]-Y[i,j])**2)
    }
  if (min(distance1,distance2,distance3,distance4,distance5)==distance1){
    testcluster<-append(testcluster,1)
  }
  if (min(distance1,distance2,distance3,distance4,distance5)==distance2){
    testcluster<-append(testcluster,2)
  }
  if (min(distance1,distance2,distance3,distance4,distance5)==distance3){
    testcluster<-append(testcluster,3)
  }
  if (min(distance1,distance2,distance3,distance4,distance5)==distance4){
    testcluster<-append(testcluster,4)
  }
  if (min(distance1,distance2,distance3,distance4,distance5)==distance5){
    testcluster<-append(testcluster,5)
  }
}
testcluster
#
tc<-as.matrix(testcluster)
newtestset<-cbind(newt,tc)
tcluster1<- newtestset[tc ==1,]
tcluster2<- newtestset[tc ==2,]
tcluster3<- newtestset[tc ==3,]
tcluster4<- newtestset[tc ==4,]
tcluster5<- newtestset[tc ==5,]

#used randomforest to do test
trf_pred1 = predict(rf_class1, newdata = tcluster1[1:10],type = "class")
trfcm1<-table(tcluster1[,11],trf_pred1)
trfcm1
trf_pred2 = predict(rf_class2, newdata = tcluster2[1:10],type = "class")
trfcm2<-table(tcluster2[,11],trf_pred2)
trfcm2
trf_pred3 = predict(rf_class3, newdata = tcluster3[1:10],type = "class")
trfcm3<-table(tcluster3[,11],trf_pred3)
trfcm3
trf_pred4 = predict(rf_class4, newdata = tcluster4[1:10],type = "class")
trfcm4<-table(tcluster4[,11],trf_pred4)
trfcm4
trf_pred5 = predict(rf_class5, newdata = tcluster5[1:10],type = "class")
trfcm5<-table(tcluster5[,11],trf_pred5)
trfcm5
###add how to plot cluster center change for low dimention
plot(x,y,col=kmeans(dataset,5)$cluster, pch=19, cex=2)
