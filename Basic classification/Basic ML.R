library(caTools)
set.seed(456605)
split = sample.split(mRMR$label, SplitRatio = 0.8)
training_set = subset(mRMR, split == TRUE)
testset = subset(mRMR, split == FALSE)

#logistic
l_classifier <- glm(formula = label ~.,
                    family = binomial,
                    data = training_set)
prob_pred = predict(l_classifier, type = 'response', 
                    newdata = testset[-11])
l_pred = ifelse(prob_pred >= 0.5, 1, 0)
lcm = table(testset[, 11], l_pred)
lcm
l_accuracy<- sum(diag(lcm))/sum(lcm)
l_accuracy
N_A<-lcm[2,2]/(lcm[1,2]+lcm[2,2])
P_A<-lcm[1,1]/(lcm[1,1]+lcm[2,1])
GA<-sqrt(N_A*P_A)
GA
#logistic_LASSO
l_classifier <- glm(formula = label ~.,
                    family = binomial,
                    data = training_set)
prob_pred = predict(l_classifier, type = 'response', 
                    newdata = testset[-11])
l_pred = ifelse(prob_pred >= 0.5, 1, 0)
lcm = table(testset[, 11], l_pred)
lcm
l_accuracy<- sum(diag(lcm))/sum(lcm)
l_accuracy
N_A<-lcm[2,2]/(lcm[1,2]+lcm[2,2])
P_A<-lcm[1,1]/(lcm[1,1]+lcm[2,1])
GA<-sqrt(N_A*P_A)
GA




#SVM
library(e1071)
s_classifier <- svm(formula = label ~.,
                    data = training_set,
                    type = 'C-classification',
                    kernel = 'linear')
# Predicting the Test set results
s_pred = predict(s_classifier, newdata = testset[-11])

# Making the Confusion Matrix
scm = table(testset[, 11], s_pred)
scm
s_accuracy<- sum(diag(scm))/sum(scm)
s_accuracy
sN_A<-scm[2,2]/(scm[1,2]+scm[2,2])
sP_A<-scm[1,1]/(scm[1,1]+scm[2,1])
sGA<-sqrt(sN_A*sP_A)
sGA


#randomForest
library(randomForest)
set.seed(156)
training_set$label<-as.factor(training_set$label)
rf_class<-randomForest(label~.,
                       data = training_set,
                       ntree=500,
                       importance=T,
                       do.trace= 50,type="classification")
plot(rf_class)
round(importance(rf_class),2)
rf_pred = predict(rf_class, newdata = testset,type = "class")
#rf_pred = ifelse(rf_pred >= 0.5, 1, 0)
#rf_pred<-as.factor(unlist(rf_pred))
rfcm<-table(testset[,11],rf_pred)
rfcm
rf_accuracy<- sum(diag(rfcm))/sum(rfcm)
rf_accuracy
rfN_A<-rfcm[2,2]/(rfcm[1,2]+rfcm[2,2])
rfP_A<-rfcm[1,1]/(rfcm[1,1]+rfcm[2,1])
rfGA<-sqrt(rfN_A*rfP_A)
rfGA

#compartion
lcm
scm
tcm
rfcm
l_accuracy<- sum(diag(lcm))/sum(lcm)
s_accuracy<- sum(diag(scm))/sum(scm)
t_accuracy<- sum(diag(tcm))/sum(tcm)
rf_accuracy<-sum(diag(rfcm))/sum(rfcm)
l_accuracy
s_accuracy
t_accuracy
rf_accuracy




#C5.0
library(C50)
# c50pred<-C5.0(x=training_set[,-17],
#               y=as.factor(training_set$label1),
#               trials = 100,
#               data = training_set)
# c_pred = predict(c50pred, newdata = test_set,type = "class")
# ccm<-table(test_set$label1,c_pred)
# ccm
# c_prec<- ccm[2,2]/(ccm[1,2]+ccm[2,2])
# c_recall<-ccm[2,2]/(ccm[2,1]+ccm[2,2])
# cF1<- 2*(c_prec*c_recall)/(c_prec+c_recall)
# cF1
# c_accuracy<- sum(diag(ccm))/sum(ccm)
# c_accuracy
# new_pred = predict(c50pred, newdata = dataset2016, type = "class")
# ncm<-table(dataset2016$label1, new_pred)
# ncm
# n_prec<- ncm[2,2]/(ncm[1,2]+ncm[2,2])
# n_recall<-ncm[2,2]/(ncm[2,1]+ncm[2,2])
# nF1<- 2*(n_prec*n_recall)/(n_prec+n_recall)
# nF1


# #DT
# library(rpart)
# 
# training_set$label1<-as.factor(training_set$label1)
# t_classifier = rpart(formula = label1 ~.,
#                      method = "class",
#                    data = training_set)
# t_pred = predict(t_classifier, newdata = test_set[-17],type = "class")
# tcm = table(test_set$label1, t_pred)
# tcm
# 
# t_prec<- tcm[2,2]/(tcm[1,2]+tcm[2,2])
# t_recall<-tcm[2,2]/(tcm[2,1]+tcm[2,2])
# tF1<- 2*(t_prec*t_recall)/(t_prec+t_recall)
# tF1
# t_accuracy<- sum(diag(tcm))/sum(tcm)
# t_accuracy