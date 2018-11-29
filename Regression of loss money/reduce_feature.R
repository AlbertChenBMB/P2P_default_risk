#reduce frome mrmr
#dataset with mRMR
s_m_dataset<-m_dataset[-c(6,8,10)]
set.seed(37)
split = sample.split(s_m_dataset, SplitRatio = 0.70)
s_m_training_set = subset(s_m_dataset, split == TRUE)
s_m_testset = subset(s_m_dataset, split == FALSE)
s_m_L_train<-s_m_training_set[-c(8,9)]
s_m_C_train<-s_m_training_set[-c(8,10)]
s_m_testset<-na.omit(s_m_testset)

#LR
library(caret)
s_m_l_classifier <- glm(formula = payback_rate ~.,
                      family = binomial,
                      data = s_m_L_train)
s_m_prob_pred = predict(s_m_l_classifier, type = 'response', 
                      newdata = s_m_testset[-c(8:10)])
RMSE( s_m_testset[10],s_m_prob_pred )

s_m_l_pred = ifelse(s_m_prob_pred >= 0.5, 1, 0)# set threshold to classification
s_m_t_r<-cbind(s_m_testset,s_m_l_pred)#cbind classify label with testset

s_m_l_fullpay<-filter(s_m_t_r,s_m_t_r$s_m_l_pred==1)
s_m_l_default<-filter(s_m_t_r,s_m_t_r$s_m_l_pred==0)

mean(s_m_l_fullpay$gain)
mean(s_m_l_default$gain)

hist(s_m_l_fullpay$gain,xlab = "profit",main = "Fully pay predict ")
hist(s_m_l_default$gain,xlab = "profit",main = "Default predict ")

quantile(m_l_fullpay$gain)
quantile(m_l_default$gain)