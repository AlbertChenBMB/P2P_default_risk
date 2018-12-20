######################
data.train <- matrix(c(5.2, -8.1, 4.8, 8.8, -16.1, 4.1, 10.6, -7.8, 5.5, 10.4, -29.0,
                       5.0, 1.8, -19.2, 3.4, 12.7, -18.9, 3.4, 15.6, -10.6, 4.9, 1.9,
                       -25.0, 3.7, 2.2, -3.1, 3.9, 4.8, -7.8, 4.5, 7.9, -13.9, 4.8,
                       5.2, -4.5, 4.9, 0.9, -11.6, 3.0, 11.8, -2.1, 4.6, 7.9, -2.0,
                       4.8, 11.5, -9.0, 5.5, 10.6, -11.2, 4.5, 11.1, -6.1, 4.7, 12.8,
                       -1.0, 6.6, 11.3, -3.6, 5.1, 1.0, -8.2, 3.9, 14.5, -0.5, 5.7,
                       11.9, -2.0, 5.1, 8.1, -1.6, 5.2, 15.5, -0.7, 4.9, 12.4, -0.8,
                       5.2, 11.1, -16.8, 5.1, 5.1, -5.1, 4.6, 4.8, -9.5, 3.9, 13.2,
                       -0.7, 6.0, 9.9, -3.3, 4.9, 12.5, -13.6, 4.1, 8.9, -10.0,
                       4.9, 10.8, -13.5, 5.1), ncol = 3, byrow = TRUE)
colnames(data.train) <- c("inp.1", "inp.2", "out.1")

data.fit <- data.train[, -ncol(data.train)]

data.test <- matrix(c(10.5, -0.9, 5.8, -2.8, 8.5, -0.6, 13.8, -11.9, 9.8, -1.2, 11.0,
                      -14.3, 4.2, -17.0, 6.9, -3.3, 13.2, -1.9), ncol = 2, byrow = TRUE)

range.data <- matrix(apply(data.train, 2, range), nrow = 2)
## I.4 Example: Constructing an FRBS model using ANFIS
#############################################################
## Not run: method.type <- "ANFIS"

control.ANFIS <- list(num.labels = 5, max.iter = 10, step.size = 0.01, type.tnorm = "MIN",
                      type.implication.func = "ZADEH", name = "Sim-0")

object.ANFIS <- frbs.learn(data.train, range.data, method.type="ANFIS", control.ANFIS)


#############################################################
#################fbs.test
library(frbs)
#test
t<-m_L_train[1:100,]
t<-as.matrix(t)
#find the range of each features
range.data.input<-matrix(apply(t, 2, range), nrow = 2)
#Model generation
#method.type we should change to ANFIS
#ANFIS and FIR.DM: list(num.labels, max.iter, step.size,type.tnorm, type.implication.func , name)
control.ANFIS <- list(num.labels = 10, max.iter = 10, step.size = 0.01, type.tnorm = "MIN",type.snorm="MAX",
                      type.implication.func = "ZADEH")

object.frbs.w<-frbs.learn(t,range.data.input, method.type = "ANFIS",control.ANFIS)
summary(object.frbs.w)

