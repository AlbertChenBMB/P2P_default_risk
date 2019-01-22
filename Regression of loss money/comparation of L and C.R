#for all features
# regression

mean(tra.dataset$payback_rate)
testset<-na.omit(testset)
testset$loan_status<-as.numeric(testset$loan_status)
fullpat<-filter(testset,testset$loan_status==1)
summary(fullpat$payback_rate)
mean(fullpat$gain)
quantile(fullpat$gain)

library(caret)

system.time(l_classifier <-glm(formula = payback_rate ~.,
                family = binomial,
                data = L_train))
options(l_classifier)
summary(l_classifier)
prob_pred = predict(l_classifier, type = 'response', 
                    newdata = testset[-c(33:35)])
RMSE( testset[35],prob_pred )
l_pred = ifelse(prob_pred >= 0.7237, 1, 0)
t_r<-cbind(testset,l_pred)
l_fullpay<-filter(t_r,t_r$l_pred==1)
summary(l_fullpay$payback_rate)
mean(l_fullpay$payback_rate)

#SVM
#reference https://rpubs.com/skydome20/R-Note14-SVM-SVR
library(e1071)
system.time(s_l <- svm(formula = payback_rate ~.,
           data = L_train,
           type = 'eps-regression',
           kernel = 'radial'))
# Predicting the Test set results
s_pred = predict(s_l, newdata =  testset[-c(33:35)])
RMSE( testset[35],s_pred  )
s_pred = ifelse(s_pred >= 0.5, 1, 0)
s_r<-cbind(testset,s_pred)
s_fullpay<-filter(s_r,s_r$s_pred==1)
summary(s_fullpay$payback_rate)
############
#randomForest
library(randomForest)
set.seed(156)
#training_set$label<-as.factor(m_L_train$payback_rate)
#names problems
names(testset)[12]<-"verification_status_Source_Verified"
names(L_train)[12]<-"verification_status_Source_Verified"
names(L_train)[11]<-"verification_status_Not_Verified"
names(testset)[11]<-"verification_status_Not_Verified"
system.time(rf_l<-randomForest(payback_rate~.,
                   data = L_train,
                   ntree=200,
                   importance=T,
                   do.trace= 10
                   ))
plot(rf_l)
round(importance(rf_l),2)
rf_pred = predict(rf_l, newdata = testset[-c(33:35)])
RMSE( testset[35],rf_pred  )

rf_pred = ifelse(rf_pred >= 0.7237, 1, 0)
r_r<-cbind(testset,rf_pred)
r_fullpay<-filter(r_r,r_r$rf_pred==1)
summary(r_fullpay$payback_rate)
#CHAID

# Decision Tree: CHAID

install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(rpart)
#anova for regression tree
system.time(cart  <- rpart(payback_rate~. ,
               method = "anova",
                  data=L_train))

rpart.plot::rpart.plot(cart)
plotcp(cart)
cart_pred = predict(cart, newdata = testset[-c(33:35)])
RMSE( testset[35],rf_pred  )

cart_pred = ifelse(rf_pred >= 0.5, 1, 0)
cart_r<-cbind(testset,cart_pred)
cart_fullpay<-filter(cart_r,cart_r$cart_pred==1)
summary(cart_fullpay$payback_rate)
