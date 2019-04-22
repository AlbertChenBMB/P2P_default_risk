#for all feature selection

# mRMR by regression label
library(mRMRe)
sapply(dataset, class)
df<-dataset[-c(33,36,37,39)]
names(df)
unfactorize<- c(1:35)
df[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(df[,x])))
# change to mRMR.data
m_data <- mRMR.data(data = data.frame(df))
#target_indices is loan_statule, feature_count is the number we want to select
set.seed(335)
#label as loan status or NRR will have different result
results <- mRMR.classic("mRMRe.Filter", data = m_data, target_indices = 35,
                        feature_count = 10)
solutions(results)
mRMRresut<-names(df[c(3,2,35,9,13,28,34,24,6,23)])
