## mRMR
library(mRMRe)
set.seed(342)

df<-na.omit(tra.dataset[1:38])
df<-df[-c(5,34)]
sapply(df, class) 
unfactorize<- c(1:36)
df[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(df[,x])))

# change to mRMR.data
f_data <- mRMR.data(data = data.frame(df))
#target_indices is loan_statule, feature_count is the number we want to select
results <- mRMR.classic("mRMRe.Filter", data = f_data, target_indices = 33,
                        feature_count = 10)
solutions(results)

mRMRresut<-names(df[c(3,9,20,26,27,24,6,14,28,23)])
#get the results 
mrmatrix<-results@mi_matrix
