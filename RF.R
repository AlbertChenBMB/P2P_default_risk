library(randomForest)
library(caret)
set.seed(998)
rf_class<-randomForest(loan_status~.,
                       data = training_set,
                       ntree=90,
                       importance=T,
                       mtry = 19,
                       do.trace= 30,type="classification")
plot(rf_class)
round(importance(rf_class),2)
# t<-as.data.frame(training_set$loan_status)
# out<-as.data.frame(rf_class$predicted)
# write.csv(out,"trainingout")
# write.csv(t,"t")

rf_pred = predict(rf_class, newdata = testset,type = "class")
#rf_pred = ifelse(rf_pred >= 0.5, 1, 0)
#rf_pred<-as.factor(unlist(rf_pred))
rfcm<-table(testset[,19],rf_pred)
rfcm
rf_prec<- rfcm[2,2]/(rfcm[1,2]+rfcm[2,2])
rf_recall<-rfcm[2,2]/(rfcm[2,1]+rfcm[2,2])
rfF1<- 2*(rf_prec*rf_recall)/(rf_prec+rf_recall)
rfF1
rf_accuracy<- sum(diag(rfcm))/sum(rfcm)
rf_accuracy
