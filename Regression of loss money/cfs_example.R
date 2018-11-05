#cfs example
#reference:https://cran.r-project.org/web/packages/FSelector/FSelector.pdf
library(FSelector)
data(iris)
subset<-cfs(Species~.,iris)
f<-as.simple.formula(subset,"Species")
print(f)