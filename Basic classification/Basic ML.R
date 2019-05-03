#C train, target 33

#logistic
C_classifier <- glm(formula = loan_status ~.,
                    family = binomial,
                    data = C_train)
C_pred = predict(C_classifier, type = 'response', 
                    newdata = testset[-c(33,36,37,38,39)])
Cpred = ifelse(C_pred >= 0.5, 1, 0)
ccm = table(testset[, 33], Cpred)
ccm
accuracy<- sum(diag(ccm))/sum(ccm)
accuracy
N_A<-ccm[2,2]/(ccm[1,2]+ccm[2,2])
P_A<-ccm[1,1]/(ccm[1,1]+ccm[2,1])
GA<-sqrt(N_A*P_A)
GA

#SVM
library(e1071)
s_classifier <- svm(formula = loan_status ~.,
                    data = C_train,
                    type = 'C-classification',
                    kernel = 'radial')
# Predicting the Test set results
s_pred = predict(s_classifier, newdata = testset[-11])

# Making the Confusion Matrix
scm = table(testset[, 11], s_pred)
scm
s_accuracy<- sum(diag(scm))/sum(scm)
s_accuracy
sN_A<-scm[2,2]/(scm[1,2]+scm[2,2])
sP_A<-scm[1,1]/(scm[1,1]+scm[2,1])
sGA<-sqrt(sN_A*sP_A)
sGA


#randomForest
library(randomForest)
set.seed(156)
C_train$loan_status<-as.factor(C_train$loan_status)
rf_class<-randomForest(loan_status~.,
                       data = C_train,
                       ntree=300,
                       importance=T,
                       do.trace= 10,type="classification")
plot(rf_class)
round(importance(rf_class),2)
rf_pred = predict(rf_class, newdata  = testset[-c(33,36,37,38,39)],type = "class")
#rf_pred = ifelse(rf_pred >= 0.5, 1, 0)
#rf_pred<-as.factor(unlist(rf_pred))
rfcm<-table(testset[,11],rf_pred)
rfcm
rf_accuracy<- sum(diag(rfcm))/sum(rfcm)
rf_accuracy
rfN_A<-rfcm[2,2]/(rfcm[1,2]+rfcm[2,2])
rfP_A<-rfcm[1,1]/(rfcm[1,1]+rfcm[2,1])
rfGA<-sqrt(rfN_A*rfP_A)
rfGA
# Keras
library(keras)
library(tensorflow)
library(tibble)
library(ggplot2)
train_data<-as.matrix(L_train[1:35])
train_labels<-as.matrix(L_train[36])
train_df <- as_tibble(train_data)
testset<-na.omit(testset)
test_data<-as.matrix(testset[1:35])
test_labels<-as.matrix(testset[37])
#build model
build_model <- function() {
  
  model <- keras_model_sequential() %>%
    
    layer_dense(units = 18, activation = "sigmoid",
                input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = "adam",
    metrics = c("accuracy")
  )
  
  model
}

model <- build_model()
model %>% summary()
# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    
epochs <- 50

# Fit the model and store training stats
system.time(history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  batch_size= 10,
  validation_split = 0.8,
  verbose = 0,
  callbacks = list(print_dot_callback)
))

plot(history, metrics = "mean_squared_error", smooth = FALSE) +
  coord_cartesian(ylim = c(0, 0.1))

test_predictions <- model %>% predict(test_data)
RMSE( test_predictions,testset[37] )
nn_pred = ifelse(test_predictions >= 0.5, 1, 0)
n_r<-cbind(testset,nn_pred)
n_fullpay<-filter(n_r,n_r$nn_pred==1)
summary(n_fullpay$NRR)
nrow(n_fullpay)/nrow(testset)
summary(n_fullpay$ROI)
#compartion
lcm
scm
tcm
rfcm
l_accuracy<- sum(diag(lcm))/sum(lcm)
s_accuracy<- sum(diag(scm))/sum(scm)
t_accuracy<- sum(diag(tcm))/sum(tcm)
rf_accuracy<-sum(diag(rfcm))/sum(rfcm)
l_accuracy
s_accuracy
t_accuracy
rf_accuracy

