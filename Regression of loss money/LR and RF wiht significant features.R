#for LR features and RF features
#LR
#top 10 term, int_rate, emp_length, 
#       home_Mortgage, home_own, verification_not verifid,
#       dti,delinq_2yr,inq_last_6mths,revol_util
LR_Sig<-L_train[,c(2,3,5,7,8,11,26,27,28,31,34)]
LR_test<-testset[,c(2,3,5,7,8,11,26,27,28,31)]
library(caret)
l_sig_reg <- glm(formula = payback_rate ~.,
                    family = binomial,
                    data = LR_Sig)
summary(l_sig_reg)
LR_pred = predict(l_sig_reg, type = 'response', 
                    newdata = LR_test)
RMSE( testset[36],LR_pred )
l_S = ifelse(LR_pred >= 0.5, 1, 0)
LS<-cbind(testset,l_S)
l_s_fullpay<-filter(LS,LS$l_S==1)
summary(l_s_fullpay$payback_rate)

#RF
#top 10 term, int_rate, installment,emp_length 
#       annual_inc, open_acc,revol_util, RTI,
#       
RF_Sig<-L_train[,c(2,3,10,26,29,31,32,34)]
RF_test<-testset[,c(2,3,10,26,29,31,32)]
library(randomForest)
set.seed(156)
s_rf_l<-randomForest(payback_rate~.,
                   data = RF_Sig,
                   ntree=200,
                   importance=T,
                   do.trace= 10)
plot(s_rf_l)
round(importance(s_rf_l),2)
s_rf_pred = predict(s_rf_l, newdata = RF_test)
RMSE( testset[35],s_rf_pred  )

s_rf_pred = ifelse(s_rf_pred >= 0.7237, 1, 0)
s_r_r<-cbind(testset,s_rf_pred)

s_r_fullpay<-filter(s_r_r, s_r_r$s_rf_pred==1)

summary(s_r_fullpay$payback_rate)
################
l_sig_reg <- glm(formula = payback_rate ~.,
                 family = binomial,
                 data = RF_Sig)
summary(l_sig_reg)
LR_pred = predict(l_sig_reg, type = 'response', 
                  newdata = RF_test)
RMSE( testset[36],LR_pred )
l_S = ifelse(LR_pred >= 0.5, 1, 0)
LS<-cbind(testset,l_S)
l_s_fullpay<-filter(LS,LS$l_S==1)
summary(l_s_fullpay$payback_rate)
