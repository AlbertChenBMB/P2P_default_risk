###for LASSO

# for LASSO
l_classifier <- glm(formula = payback_rate ~.,
                      family = binomial,
                      data = LASSO)
m_prob_pred  = predict(l_classifier, type = 'response',
                   newdata = l_test[-c(28)])


summary(l_classifier)

RMSE( m_prob_pred,l_test[28] )

l_pred = ifelse(m_prob_pred >= 0.5, 1, 0)# set threshold to classification
l_t_r<-cbind(l_test,l_pred)#cbind classify label with testset

l_fullpay<-filter(l_t_r,l_t_r$l_pred==1)
summary(l_fullpay)
