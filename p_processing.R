library(dplyr)
library(data.table)
library(dummies)
#read data
dataset1<-read.csv("LoanStats_2016Q1.csv")
dataset2<-read.csv("LoanStats_2016Q2.csv")
dataset3<-read.csv("LoanStats_2016Q3.csv")
dataset4<-read.csv("LoanStats_2016Q4.csv")
#merge data
dataset2016<-Reduce(function(x, y) merge(x, y, all=TRUE), list(dataset1, dataset2, dataset3,dataset4))
#select necessary variables
dataset2016<-select(dataset2016,c(1, 4 ,5, 6, 7, 8, 10, 11, 12, 14 ,15 ,19 ,23 ,24, 25, 26 ,29 ,31 ,32, 33))
#filter loan status to remine Full pay, default, charge off
dataset2016<-filter(dataset2016,loan_status =="Fully Paid"|loan_status =="Charged Off"|loan_status =="Default")

class(dataset2016)
tbl_df(dataset2016)
class(dataset)
# #time problem rember change in csv
dataset2016$issue_d<-as.Date(dataset2016$issue_d)
dataset2016$earliest_cr_line<-as.Date(dataset2016$earliest_cr_line)
#creat new variable call credit age
dataset2016<-mutate(dataset2016,credit_age = dataset2016$issue_d-dataset2016$earliest_cr_line)
#log to dataset
dataset2016$credit_age<-log(as.numeric(dataset2016$credit_age))
#replace 0 to the NaN result
dataset2016$credit_age[is.na(dataset2016$credit_age)]<-0
#delet two old variables
dataset2016$issue_d<-NULL
dataset2016$earliest_cr_line<-NULL
#creat new variable call income to payment ratio ITP
dataset2016<-mutate(dataset2016,mic = dataset2016$annual_inc/12,ITP = (dataset2016$installment/mic))
dataset2016$ITP<-log(dataset2016$ITP)
#creat new variable call income to payment ratio RTI
dataset2016<-mutate(dataset2016,RTI =dataset2016$revol_bal/mic)
dataset2016$RTI<-log(dataset2016$RTI)
#delet old variable
dataset2016$mic<-NULL
dataset2016$revol_bal<-NULL
#normalize to annual income, total amount
dataset2016$annual_inc<-log(dataset2016$annual_inc)
dataset2016$loan_amnt<-log(dataset2016$loan_amnt)


# Encoding the target feature as factor
dataset2016$term = factor(dataset2016$term,
                      levels = c(" 36 months"," 60 months"),
                      labels = c(3, 5))
dataset2016$emp_length = factor(dataset2016$emp_length,
                            levels= c("n/a","< 1 year",
                                      "1 year","2 years",
                                      "3 years","4 years",
                                      "5 years","6 years",
                                      "7 years","8 years",
                                      "9 years","10 years", "10+ years"),
                            labels=c(0,0.5,1,2,3,4,5,6,7,8,9,10,20))
#replace loanstatuc by 1,0
loanStatus<-c("Fully Paid"=0,"Default"=1,"Charged Off"=1)
dataset2016$loan_status <- loanStatus[dataset2016$loan_status]
#dummy present
dataset2016 <- dummy.data.frame(dataset2016, names=c("home_ownership"), sep="_")
dataset2016 <- dummy.data.frame(dataset2016, names=c("verification_status"), sep="_")
dataset2016 <- dummy.data.frame(dataset2016, names=c("purpose"), sep="_")
dataset2016$grade<-NULL
dataset2016$sub_grade<-NULL
###
#write.csv(dataset,"newall2016.csv")


#######verify data
testset1<-read.csv("LoanStats_2017Q1.csv")
testset2<-read.csv("LoanStats_2017Q2.csv")
testset<-Reduce(function(x, y) merge(x, y, all=TRUE), list(testset1, testset2))

testset<-testset[1:33]
#2 3 7 8 9 16 17 18 20 21 22 27 28 
testset<-testset[,-c(2,3,7,8,9,16,17,18,20,21,22,27,28)]
class(testset)
tbl_df(testset)
class(testset)
# Encoding the target feature as factor
testset$term = factor(testset$term,
                      levels = c(" 36 months"," 60 months"),
                      labels = c(3, 5))
testset$emp_length = factor(testset$emp_length,
                            levels= c("n/a","< 1 year",
                                      "1 year","2 years",
                                      "3 years","4 years",
                                      "5 years","6 years",
                                      "7 years","8 years",
                                      "9 years","10 years", "10+ years"),
                            labels=c(0,0.5,1,2,3,4,5,6,7,8,9,10,20))
testset$loan_status = factor(testset$loan_status,
                             levels = c("Fully Paid",
                                        "Default",
                                        "Charged Off"),
                             labels = c(0, 1,1))

testset <- dummy.data.frame(testset, names=c("home_ownership"), sep="_")
testset <- dummy.data.frame(testset, names=c("verification_status"), sep="_")
testset <- dummy.data.frame(testset, names=c("purpose"), sep="_")

write.csv(testset,"test2017.csv")

