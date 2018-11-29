#for featureselection
# regression
#C_train L_train testset 
m_testset<-na.omit(m_testset)
library(caret)
m_l_classifier <- glm(formula = payback_rate ~.,
                    family = binomial,
                    data = m_L_train)
m_prob_pred = predict(m_l_classifier, type = 'response', 
                    newdata = m_testset[-c(11:13)])
RMSE( m_testset[13],m_prob_pred )

m_l_pred = ifelse(m_prob_pred >= 0.5, 1, 0)# set threshold to classification
m_t_r<-cbind(m_testset,m_l_pred)#cbind classify label with testset

m_l_fullpay<-filter(m_t_r,m_t_r$m_l_pred==1)
m_l_default<-filter(m_t_r,m_t_r$m_l_pred==0)

mean(m_l_fullpay$gain)
mean(m_l_default$gain)

hist(m_l_fullpay$gain)
hist(m_l_default$gain)

quantile(m_l_fullpay$gain)
quantile(m_l_default$gain)
#Gain
m_g_classifier <- glm(formula = gain ~.,
                      family = binomial,
                      data = m_g_train)
g_prob_pred = predict(m_g_classifier, type = 'response', 
                      newdata = m_testset[-c(11:13)])
#RMSE( m_testset[11],m_prob_pred )

m_g_pred = ifelse(m_prob_pred >= 0.5, 1, 0)# set threshold to classification
m_g_r<-cbind(m_testset,m_g_pred)#cbind classify label with testset

m_g_fullpay<-filter(m_g_r,m_g_r$m_g_pred==1)
m_g_default<-filter(m_g_r,m_g_r$m_g_pred==0)

mean(m_g_fullpay$gain)
mean(m_g_default$gain)

hist(m_g_fullpay$gain,xlab = "profit",main = "Fully pay predict ")
hist(m_g_default$gain,xlab = "profit",main = "Default predict ")

quantile(m_g_fullpay$gain)
quantile(m_g_default$gain)


# classification

m_C_classifier <- glm(formula = loan_status ~.,
                    family = binomial,
                    data = m_C_train)
m_C_pred = predict(m_C_classifier, type = 'response', 
                    newdata = m_testset[-c(11:13)])
RMSE( m_testset[13],m_C_pred  )
m_c_pred = ifelse(prob_pred >= 0.5, 1, 0)# set threshold to classification
m_c_r<-cbind(testset,c_pred)

m_c_fullpay<-filter(c_r,c_r$c_pred==1)
m_c_default<-filter(c_r,c_r$c_pred==0)

mean(m_c_fullpay$gain)
mean(m_c_default$gain)

hist(m_c_fullpay$gain,xlab = "profit",main = "Fully pay predict ")
hist(m_c_default$gain,xlab = "profit",main = "Default predict ")

quantile(m_c_fullpay$gain)
quantile(m_c_default$gain)
#CM
ccm = table(testset[, 34], c_pred)
c_accuracy<- sum(diag(ccm))/sum(ccm)
c_accuracy
cN_A<-ccm[2,2]/(ccm[1,2]+ccm[2,2])
cP_A<-ccm[1,1]/(ccm[1,1]+ccm[2,1])
cGA<-sqrt(cN_A*cP_A)
cGA

#SVM
library(e1071)
s_classifier <- svm(formula = payback_rate ~.,
                    data = m_L_train,
                    type = 'eps-regression',
                    kernel = 'linear')
# Predicting the Test set results
s_pred = predict(s_classifier, newdata =  m_testset[-c(11:13)])
m_s_pred = ifelse(s_pred >= 0.5, 1, 0)
m_s_r<-cbind(m_testset,m_s_pred)
RMSE( m_testset[13],s_pred  )

m_s_fullpay<-filter(m_s_r,m_s_r$m_s_pred==1)
m_s_default<-filter(m_s_r,m_s_r$m_s_pred==0)

mean(m_s_fullpay$gain)
mean(m_s_default$gain)

hist(m_s_fullpay$gain)
hist(m_s_default$gain)


quantile(m_s_fullpay$gain)
quantile(m_s_default$gain)

#Random forest
#randomForest
library(randomForest)
set.seed(156)
#training_set$label<-as.factor(m_L_train$payback_rate)
rf_class<-randomForest(payback_rate~.,
                       data = na.omit(m_L_train),
                       ntree=100,
                       importance=T,
                       do.trace= 10)
plot(rf_class)
round(importance(rf_class),2)
rf_pred = predict(rf_class, newdata = m_testset[-c(11:13)])
m_rf_pred = ifelse(rf_pred >= 0.5, 1, 0)
m_r_r<-cbind(m_testset,m_rf_pred)
RMSE( m_testset[13],rf_pred  )

m_r_fullpay<-filter(m_r_r,m_r_r$m_rf_pred==1)
m_r_default<-filter(m_r_r,m_r_r$m_rf_pred==0)

mean(m_r_fullpay$gain)
mean(m_r_default$gain)

hist(m_r_fullpay$gain)
hist(m_r_default$gain)


quantile(m_r_fullpay$gain)
quantile(m_r_default$gain)
