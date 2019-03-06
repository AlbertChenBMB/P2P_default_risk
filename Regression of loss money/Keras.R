#for keras
#train data and testdata
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




build_model <- function() {
        
        model <- keras_model_sequential() %>%
                
                layer_dense(units = 18, activation = "relu",
                            input_shape = dim(train_data)[2]) %>%
                layer_dense(units = 18, activation = "sigmoid",
                            input_shape = dim(train_data)[2]) %>%
                layer_dense(units = 1)
        
        model %>% compile(
                loss = "mse",
                optimizer = optimizer_rmsprop(),
                metrics = list("mean_squared_error")
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

# The patience parameter is the amount of epochs to check for improvement.
# early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)
# 
# model <- build_model()
# history <- model %>% fit(
#         train_data,
#         train_labels,
#         epochs = epochs,
#         validation_split = 0.2,
#         verbose = 0,
#         callbacks = list(early_stop, print_dot_callback)
# )
# 
# plot(history, metrics = "mean_squared_error", smooth = FALSE) +
#         coord_cartesian(xlim = c(0, 150), ylim = c(0, 5))
# 
# #################################################################
# c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))
# 
# paste0("Mean absolute error on test set: $", sprintf("%.2f", mae ))
####################
test_predictions <- model %>% predict(test_data)
RMSE( test_predictions,testset[37] )
nn_pred = ifelse(test_predictions >= 0.5, 1, 0)
n_r<-cbind(testset,nn_pred)
n_fullpay<-filter(n_r,n_r$nn_pred==1)
summary(n_fullpay$NRR)
nrow(n_fullpay)/nrow(testset)
summary(n_fullpay$ROI)
###############
#for mRMR
train_data<-as.matrix(m_L_train[1:10])
train_labels<-as.matrix(m_L_train[11])
train_df <- as_tibble(train_data)

test_data<-as.matrix(m_testset[1:10])
test_labels<-as.matrix(m_testset[13])




build_model <- function() {
        
        model <- keras_model_sequential() %>%
                layer_dense(units = 16, activation = "relu",
                            input_shape = dim(train_data)[2]) %>%
                
                layer_dense(units = 1)
        
        model %>% compile(
                loss = "mse",
                optimizer = optimizer_rmsprop(),
                metrics = list("mean_squared_error")
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
history <- model %>% fit(
        train_data,
        train_labels,
        epochs = epochs,
        batch_size= 10,
        validation_split = 0.8,
        verbose = 0,
        callbacks = list(print_dot_callback)
)



plot(history, metrics = "mean_squared_error", smooth = FALSE) +
        coord_cartesian(ylim = c(0, 1))

# The patience parameter is the amount of epochs to check for improvement.
# early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)
# 
# model <- build_model()
# history <- model %>% fit(
#         train_data,
#         train_labels,
#         epochs = epochs,
#         validation_split = 0.2,
#         verbose = 0,
#         callbacks = list(early_stop, print_dot_callback)
# )
# 
# plot(history, metrics = "mean_squared_error", smooth = FALSE) +
#         coord_cartesian(xlim = c(0, 150), ylim = c(0, 5))
# 
# #################################################################
# c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))
# 
# paste0("Mean absolute error on test set: $", sprintf("%.2f", mae ))
####################
m_test_predictions <- model %>% predict(test_data)
RMSE( m_test_predictions,m_testset[13] )
m_nn_pred = ifelse(m_test_predictions >= 0.5, 1, 0)
m_n_r<-cbind(m_testset,m_nn_pred)
m_n_fullpay<-filter(m_n_r,m_n_r$m_nn_pred==1)
summary(m_n_fullpay$payback_rate)
