## mRMR
library(mRMRe)
sapply(dataset, class)
df<-dataset[-c(36,38,39)]
names(df)
unfactorize<- c(1:36)
df[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(df[,x])))

# change to mRMR.data
m_data <- mRMR.data(data = data.frame(df))
#target_indices is loan_statule, feature_count is the number we want to select
set.seed(335)
#label as loan status or NRR will have different result
results <- mRMR.classic("mRMRe.Filter", data = m_data, target_indices = 36,
                        feature_count = 10)
solutions(results)
mRMRresut<-names(df[c(3,2,35,9,13,28,34,24,6,23)])

#LASSO
library(glmnet)
library(caTools)
set.seed(456605)
LA<-dataset[-c(36,38,37)]
split = sample.split(LA$loan_status, SplitRatio = 0.8)
training_set = subset(LA, split == TRUE)
testset = subset(LA, split == FALSE)
LA$loan_status<-as.factor(LA$loan_status)
lasso = glmnet(x = as.matrix(df[, -36]), 
               y = df[, 36], 
               alpha = 1,
               family = "binomial")

plot(lasso, xvar='lambda', main="Lasso")

#
cv.lasso = cv.glmnet(x = as.matrix(training_set[, -34]), 
                     y = training_set[, 34], 
                     alpha = 1,
                     family = "binomial")

coef(cv.lasso, s = "lambda.min")
best.lambda = cv.lasso$lambda.min
best.lambda
plot(lasso, xvar='lambda', main="Lasso")
abline(v=log(best.lambda), col="blue", lty=5.5 )
# 如果要取出這些重要變數的名稱，可以這樣寫：
select.ind = which(coef(cv.lasso, s = "lambda.min") != 0)
select.ind = select.ind[-1]-1 # remove `Intercept` and 平移剩下的ind
select.ind # 第幾個變數是重要的 (不看 `Intercept`)