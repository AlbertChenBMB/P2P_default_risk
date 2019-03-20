#for all features
#names problems
names(testset)[12]<-"verification_status_Source_Verified"
names(L_train)[12]<-"verification_status_Source_Verified"
names(L_train)[11]<-"verification_status_Not_Verified"
names(testset)[11]<-"verification_status_Not_Verified"
# regression

library(MLmetrics)
library(caret)

system.time(l_classifier <-glm(formula = NRR ~.,
                family = binomial,
                data = L_train))
summary(l_classifier)
prob_pred = predict(l_classifier, type = 'response', 
                    newdata = testset[-c(36:38)])
RMSE( testset[37],prob_pred )
l_pred = ifelse(prob_pred >= 0.5, 1, 0)
t_r<-cbind(testset,l_pred)
l_fullpay<-filter(t_r,t_r$l_pred==1)
summary(l_fullpay$NRR)
mean(l_fullpay$NRR)
sd(l_fullpay$NRR)
mean(l_fullpay$ROI)
nrow(l_fullpay)/nrow(testset)

#SVM
#reference https://rpubs.com/skydome20/R-Note14-SVM-SVR
library(e1071)
system.time(s_l <- svm(formula = NRR ~.,
           data = L_train,
           type = 'eps-regression',
           kernel = 'radial'))
# Predicting the Test set results
s_pred = predict(s_l, newdata =  testset[-c(36:38)])
RMSE( testset[37],s_pred  )
s_pred = ifelse(s_pred >= 0.5, 1, 0)
s_r<-cbind(testset,s_pred)
s_fullpay<-filter(s_r,s_r$s_pred==1)
summary(s_fullpay$NRR)
mean(s_fullpay$NRR)
mean(s_fullpay$ROI)
nrow(s_fullpay)/nrow(testset)
############
#randomForest
library(randomForest)
set.seed(156)
#training_set$label<-as.factor(m_L_train$payback_rate)

system.time(rf_l<-randomForest(NRR~.,
                   data = L_train,
                   ntree=200,
                   importance=T,
                   do.trace= 10
                   ))
plot(rf_l)
round(importance(rf_l),2)
rf_pred = predict(rf_l, newdata = testset[-c(36:38)])
RMSE( testset[37],rf_pred  )

rf_pred = ifelse(rf_pred >= 0.5, 1, 0)
r_r<-cbind(testset,rf_pred)
r_fullpay<-filter(r_r,r_r$rf_pred==1)
summary(r_fullpay$NRR)
mean(r_fullpay$NRR)
mean(r_fullpay$ROI)
nrow(r_fullpay)/nrow(testset)
#CHAID

# Decision Tree: CHAID

install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(rpart)
#anova for regression tree
system.time(cart  <- rpart(NRR~. ,
               method = "anova",
                  data=L_train))

rpart.plot::rpart.plot(cart)
plotcp(cart)
cart_pred = predict(cart, newdata = testset[-c(36:38)])
RMSE( testset[37],cart_pred  )

cart_pred = ifelse(cart_pred >= 0.5, 1, 0)
cart_r<-cbind(testset,cart_pred)
cart_fullpay<-filter(cart_r,cart_r$cart_pred==1)
summary(cart_fullpay$NRR)
mean(cart_fullpay$NRR)
mean(cart_fullpay$ROI)
nrow(cart_fullpay)/nrow(testset)
###################