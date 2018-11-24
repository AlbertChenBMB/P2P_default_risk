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

m_l_pred = ifelse(m_prob_pred >= 0.723, 1, 0)
m_t_r<-cbind(m_testset,m_l_pred)
m_l_fullpay<-filter(m_t_r,m_t_r$m_l_pred==1)
sum(m_l_fullpay$gain)
mean(m_l_fullpay$gain)
quantile(m_l_fullpay$gain)
#CM

# classification

m_C_classifier <- glm(formula = loan_status ~.,
                    family = binomial,
                    data = m_C_train)
m_C_pred = predict(m_C_classifier, type = 'response', 
                    newdata = m_testset[-c(11:13)])
RMSE( m_testset[13],m_C_pred  )
m_c_pred = ifelse(prob_pred >= 0.5, 1, 0)
m_c_r<-cbind(testset,c_pred)
m_c_fullpay<-filter(c_r,c_r$c_pred==1)
sum(m_c_fullpay$gain)
mean(m_c_fullpay$gain)
quantile(m_c_fullpay$gain)
#CM
ccm = table(testset[, 34], c_pred)
c_accuracy<- sum(diag(ccm))/sum(ccm)
c_accuracy
cN_A<-ccm[2,2]/(ccm[1,2]+ccm[2,2])
cP_A<-ccm[1,1]/(ccm[1,1]+ccm[2,1])
cGA<-sqrt(cN_A*cP_A)
cGA

