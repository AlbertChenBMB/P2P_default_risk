#classification for undersampling and mRMR LASSO
mRMR
LASSO
#formRMR
set.seed(37)
split = sample.split(mRMR, SplitRatio = 0.70)
m_training_set = subset(mRMR, split == TRUE)
m_testset = subset(mRMR, split == FALSE)
#for LASSO
set.seed(37)
split = sample.split(LASSO, SplitRatio = 0.70)
l_training_set = subset(LASSO, split == TRUE)
l_testset = subset(LASSO, split == FALSE)

#classification for mRMR
m_C_classifier <- glm(formula = loan_status ~.,
                    family = binomial,
                    data = m_training_set)
m_C_pred = predict(m_C_classifier, type = 'response',
                    newdata = m_testset[-c(11)])

m_C_pred = ifelse(m_C_pred >= 0.5, 1, 0)# set threshold to classification
m_C_r<-cbind(m_testset,m_C_pred)
#CM
ccm = table(m_testset[, 11], m_C_pred)
c_accuracy<- sum(diag(ccm))/sum(ccm)
c_accuracy
cN_A<-ccm[2,2]/(ccm[1,2]+ccm[2,2])
cP_A<-ccm[1,1]/(ccm[1,1]+ccm[2,1])
cGA<-sqrt(cN_A*cP_A)
cN_A
cP_A
cGA
# for LASSO
l_C_classifier <- glm(formula = loan_status ~.,
                      family = binomial,
                      data = l_training_set)
l_C_pred = predict(l_C_classifier, type = 'response',
                   newdata = l_testset[-c(28)])

l_C_pred = ifelse(l_C_pred >= 0.5, 1, 0)# set threshold to classification
l_C_r<-cbind(l_testset,l_C_pred)
#CM
lcm = table(l_testset[, 28], l_C_pred)
l_accuracy<- sum(diag(lcm))/sum(lcm)
l_accuracy
lN_A<-lcm[2,2]/(lcm[1,2]+lcm[2,2])
lP_A<-lcm[1,1]/(lcm[1,1]+lcm[2,1])
lGA<-sqrt(lN_A*lP_A)
lN_A
lP_A
lGA
#library(randomForest)
set.seed(156)
names(l_testset)[8]<-"verification_status_Verified"
names(l_training_set)[8]<-"verification_status_Verified"
names(l_training_set)[7]<-"verification_status_Not_Verified"
names(l_testset)[7]<-"verification_status_Not_Verified"
l_training_set$loan_status<-as.factor(l_training_set$loan_status)
rf_l<-randomForest(loan_status ~.,
                   data = l_training_set,
                   type="classification",
                   ntree=100,
                   importance=T,
                   do.trace= 10)
plot(rf_l)
round(importance(rf_l),2)
rf_pred = predict(rf_l, newdata = l_testset[-28],type = "class")
l_testset$loan_status<-as.factor(l_testset$loan_status)
rfcm<-table(l_testset$loan_status,rf_pred)
rfcm
rf_accuracy<- sum(diag(rfcm))/sum(rfcm)
rf_accuracy
rfN_A<-rfcm[2,2]/(rfcm[1,2]+rfcm[2,2])
rfP_A<-rfcm[1,1]/(rfcm[1,1]+rfcm[2,1])
rfGA<-sqrt(rfN_A*rfP_A)
rfGA
rfN_A
rfP_A
