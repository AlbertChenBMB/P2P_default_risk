
#############################################################
#################fbs.test
library(frbs)
#test
t<-m_L_train
t<-as.matrix(t)
t<-round(t,digits = 4)
#find the range of each features
range.data.input<-matrix(apply(t, 2, range), nrow = 2)
#Model generation
#method.type we should change to ANFIS
#ANFIS and FIR.DM: list(num.labels, max.iter, step.size,type.tnorm, type.implication.func , name)
control.ANFIS <- list(num.labels = 5, max.iter = 10, step.size = 0.01, type.tnorm = "MIN",type.snorm="MAX",
                      type.implication.func = "ZADEH", name = "Sim-0")
###########################################################
object.frbs.w<-frbs.learn(t,range.data.input, method.type = "ANFIS",control.ANFIS)
summary(object.frbs.w)

