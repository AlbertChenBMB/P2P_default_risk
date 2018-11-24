#for all features
# regression
#C_train L_train testset 

testset<-na.omit(testset)
testset$loan_status<-as.numeric(testset$loan_status)
fullpat<-filter(testset,testset$loan_status==1)
sum(fullpat$gain)
mean(fullpat$gain)
quantile(fullpat$gain)

library(caret)
l_classifier <- glm(formula = payback_rate ~.,
                    family = binomial,
                    data = L_train)
prob_pred = predict(l_classifier, type = 'response', 
                    newdata = testset[-c(33:35)])
RMSE( testset[35],prob_pred )
l_pred = ifelse(prob_pred >= 0.723, 1, 0)
t_r<-cbind(testset,l_pred)
l_fullpay<-filter(t_r,t_r$l_pred==1)
sum(l_fullpay$gain)
mean(l_fullpay$gain)
quantile(l_fullpay$gain)
#CM
lcm = table(testset[, 34], l_pred)
l_accuracy<- sum(diag(lcm))/sum(lcm)
l_accuracy
N_A<-lcm[2,2]/(lcm[1,2]+lcm[2,2])
P_A<-lcm[1,1]/(lcm[1,1]+lcm[2,1])
GA<-sqrt(N_A*P_A)
GA

# classification

C_classifier <- glm(formula = loan_status ~.,
                    family = binomial,
                    data = C_train)
C_pred = predict(C_classifier, type = 'response', 
                    newdata = testset[-c(33:35)])
RMSE( testset[35],C_pred  )
c_pred = ifelse(prob_pred >= 0.5, 1, 0)
c_r<-cbind(testset,c_pred)
c_fullpay<-filter(c_r,c_r$c_pred==1)
sum(c_fullpay$gain)
mean(c_fullpay$gain)
quantile(c_fullpay$gain)
#CM
ccm = table(testset[, 34], c_pred)
c_accuracy<- sum(diag(ccm))/sum(ccm)
c_accuracy
cN_A<-ccm[2,2]/(ccm[1,2]+ccm[2,2])
cP_A<-ccm[1,1]/(ccm[1,1]+ccm[2,1])
cGA<-sqrt(cN_A*cP_A)
cGA

