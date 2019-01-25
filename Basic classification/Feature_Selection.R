str(dataset)

## mRMR
library(mRMRe)
set.seed(342)
sapply(dataset, class) # this will show you the classes of all columns in df
df<-dataset[-c(34,36)]
unfactorize<- c(1:34)
df[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(df[,x])))



# change to mRMR.data
f_data <- mRMR.data(data = data.frame(df))
#target_indices is loan_statule, feature_count is the number we want to select
results <- mRMR.classic("mRMRe.Filter", data = f_data, target_indices = 33,
                        feature_count = 10)
solutions(results)

mRMRresut<-names(df[c(31,26,32,29,15,30,27,17,19,2)])
#get the results 
mrmatrix<-results@mi_matrix


#LASSO
library(glmnet)
#glmnet(...
# family = y是連續值，設"gaussian"；若y是二元分類，設"binomial"；若y是多元分類，設"multinomial"。
# alpha = 0(Ridge) 或 1(Lasso)。lambda = 懲罰值，也就是給權重和的限制 (跟 SVM 中的 C 概念很像)
# )
## output 01~03 is balance data
#split
#omitt na
df<-na.omit(df)
library(caTools)
set.seed(456605)
split = sample.split(df$loan_status, SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
testset = subset(df, split == FALSE)

lasso = glmnet(x = as.matrix(training_set[, -34]), 
               y = training_set[, 34], 
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
