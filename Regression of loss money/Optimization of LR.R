# find the optimial threshold for Logistic regression

system.time(l_classifier <-glm(formula = NRR ~.,
                               family = binomial,
                               data = L_train))
prob_pred = predict(l_classifier, type = 'response', 
                    newdata = testset[-c(36:38)])
RMSE( testset[37],prob_pred )

result<-matrix(0,ncol = 6,nrow = 100)

#for testset, mean of ROI is 0.9578, mean of NRR is 0.6123
for(i in 1:100){
        l_pred = ifelse(prob_pred >= i/100, 1, 0)
        t_r<-cbind(testset,l_pred)
        l_fullpay<-filter(t_r,t_r$l_pred==1)
        result[i,1]<-i/100
        result[i,2]<-mean(l_fullpay$NRR)
        result[i,3]<-mean(l_fullpay$ROI)
        result[i,4]<-nrow(l_fullpay)/nrow(testset)
        result[i,5]<-result[i,2]*result[i,4]
        result[i,6]<-result[i,3]*result[i,4]
        
}



plot(y=result[,2],x=result[,4], type="l",xlab = "lend ratio", ylab = "mean of NRR")
plot(y=result[,5],x=result[,1], type="l",ylab = "NRR*lend ratio", xlab = "threshold")

#for bino
system.time(c_classifier <-glm(formula = loan_status ~.,
                               family = binomial,
                               data = C_train))
c_prob_pred = predict(c_classifier, type = 'response', 
                    newdata = testset[-c(36:38)])
c_pred = ifelse(prob_pred >= 0.5, 1, 0)
t_r<-cbind(testset,c_pred)
fullpay<-filter(t_r,t_r$c_pred==1)
mean(fullpay$NRR)
mean(fullpay$ROI)
nrow(fullpay)/nrow(testset)






RMSE( testset[37],prob_pred )
cm = table(testset[, 38], c_pred)
cm
accuracy<- sum(diag(cm))/sum(cm)
accuracy
N_A<-cm[2,2]/(cm[1,2]+cm[2,2])
P_A<-cm[1,1]/(cm[1,1]+cm[2,1])
GA<-sqrt(N_A*P_A)
GA
