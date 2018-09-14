library(mRMRe)
set.seed(342)
# change to mRMR.data
f_data <- mRMR.data(data = data.frame(mRMRtest))
#target_indices is loan_statule, feature_count is the number we want to select
results <- mRMR.classic("mRMRe.Filter", data = f_data, target_indices = 31,
                        feature_count = 10)
solutions(results)
#get the results 
mrmatrix<-results@mi_matrix
