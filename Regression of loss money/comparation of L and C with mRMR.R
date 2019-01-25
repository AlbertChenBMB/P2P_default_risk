#for featureselection
# regression
#C_train L_train testset 

library(caret)

l_classifier <- glm(formula = payback_rate ~.,
                    family = binomial,
                    data = mRMR)
summary(l_classifier)
m_prob_pred = predict(l_classifier, type = 'response', 
                    newdata = m_test[-c(11)])
RMSE( m_prob_pred,m_test[11] )

m_l_pred = ifelse(m_prob_pred >= 0.5, 1, 0)# set threshold to classification
m_t_r<-cbind(m_testset,m_l_pred)#cbind classify label with testset

m_l_fullpay<-filter(m_t_r,m_t_r$m_l_pred==1)
summary(m_l_fullpay)

# # classification
# 
# m_C_classifier <- glm(formula = loan_status ~.,
#                     family = binomial,
#                     data = m_C_train)
# m_C_pred = predict(m_C_classifier, type = 'response', 
#                     newdata = m_testset[-c(11:13)])
# RMSE( m_testset[13],m_C_pred  )
# m_c_pred = ifelse(prob_pred >= 0.5, 1, 0)# set threshold to classification
# m_c_r<-cbind(testset,c_pred)
# 
# m_c_fullpay<-filter(c_r,c_r$c_pred==1)
# m_c_default<-filter(c_r,c_r$c_pred==0)
# 
# mean(m_c_fullpay$gain)
# mean(m_c_default$gain)
# 
# hist(m_c_fullpay$gain,xlab = "profit",main = "Fully pay predict ")
# hist(m_c_default$gain,xlab = "profit",main = "Default predict ")
# 
# quantile(m_c_fullpay$gain)
# quantile(m_c_default$gain)
# #CM
# ccm = table(testset[, 34], c_pred)
# c_accuracy<- sum(diag(ccm))/sum(ccm)
# c_accuracy
# cN_A<-ccm[2,2]/(ccm[1,2]+ccm[2,2])
# cP_A<-ccm[1,1]/(ccm[1,1]+ccm[2,1])
# cGA<-sqrt(cN_A*cP_A)
# cGA

#SVM
#reference https://rpubs.com/skydome20/R-Note14-SVM-SVR
library(e1071)
s_l <- svm(formula = payback_rate ~.,
                    data = m_L_train,
                    type = 'eps-regression',
                    kernel = 'radial')
# Predicting the Test set results
s_pred = predict(s_l, newdata =  m_testset[-c(11:13)])
m_s_pred = ifelse(s_pred >= 0.5, 1, 0)
m_s_r<-cbind(m_testset,m_s_pred)
RMSE( m_testset[13],s_pred  )

m_s_fullpay<-filter(m_s_r,m_s_r$m_s_pred==1)
summary(m_s_fullpay)

#Random forest
#randomForest
library(randomForest)
set.seed(156)
names(testset)[12]<-"verification_status_Source_Verified"
names(L_train)[12]<-"verification_status_Source_Verified"
names(L_train)[11]<-"verification_status_Not_Verified"
names(testset)[11]<-"verification_status_Not_Verified"
#training_set$label<-as.factor(m_L_train$payback_rate)
rf_l<-randomForest(payback_rate~.,
                       data = na.omit(m_L_train),
                       ntree=200,
                       importance=T,
                       do.trace= 10)
plot(rf_l)
round(importance(rf_l),2)
rf_pred = predict(rf_l, newdata = m_testset[-c(11:13)])
RMSE( m_testset[13],rf_pred  )

m_rf_pred = ifelse(rf_pred >= 0.5, 1, 0)
m_r_r<-cbind(m_testset,m_rf_pred)
m_r_fullpay<-filter(m_r_r,m_r_r$m_rf_pred==1)
summary(m_r_fullpay)


