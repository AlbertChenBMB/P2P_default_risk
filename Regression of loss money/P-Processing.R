library(dplyr)
library(data.table)
library(dummies)
#for time
library(lubridate)
#read data for 2016
dataset1<-read.csv("LoanStats_2016Q1.csv",stringsAsFactors = FALSE)
dataset2<-read.csv("LoanStats_2016Q2.csv",stringsAsFactors = FALSE)
dataset3<-read.csv("LoanStats_2016Q3.csv",stringsAsFactors = FALSE)
dataset4<-read.csv("LoanStats_2016Q4.csv",stringsAsFactors = FALSE)
#used only dataset4 to try
str(dataset4)

#merge data
#all_dataset2016<-Reduce(function(x, y) merge(x, y, all=TRUE), list(dataset1, dataset2, dataset3,dataset4))
#select necessary variables
dataset<-select(dataset4,c(1, 3,4 ,5, 6, 10, 11, 12, 13,14 ,19 ,23 ,24, 25, 26 ,29,30 ,31 ,32, 33,38,15))
#filter loan status to remine Full pay, default, charge off
dataset<-filter(dataset,loan_status =="Fully Paid"|loan_status =="Charged Off"|loan_status =="Default")
str(dataset)
tbl_df(dataset)
# #time problem rember change in csv
dataset$issue_d<-dmy(paste("01-", dataset$issue_d , sep =""))
dataset$earliest_cr_line<-dmy(paste("01-", dataset$earliest_cr_line , sep =""))
#creat new variable call credit age
dataset<-mutate(dataset,
                    credit_age = difftime(dataset$issue_d,dataset$earliest_cr_line,units="weeks"))
#scale to dataset
dataset$credit_age<-log(as.numeric(dataset$credit_age))
#replace 0 to the NaN result
dataset$credit_age[is.na(dataset$credit_age)]<-0
#delet two old variables
dataset$issue_d<-NULL
dataset$earliest_cr_line<-NULL
#creat new variable call income to payment ratio ITP
dataset<-mutate(dataset,mic = dataset$annual_inc/12,ITP = (dataset$installment/mic))
dataset$ITP<-log(dataset$ITP)
#creat new variable call income to payment ratio RTI
dataset<-mutate(dataset,RTI =dataset$revol_bal/mic)
dataset$RTI<-log(dataset$RTI)
#delet old variable
dataset$mic<-NULL
dataset$revol_bal<-NULL
#normalize to annual income, total amount
dataset$annual_inc<-log(dataset$annual_inc)
dataset$loan_amnt<-log(dataset$loan_amnt)


# Encoding the target feature as factor
dataset$term = factor(dataset$term,
                          levels = c(" 36 months"," 60 months"),
                          labels = c(3, 5))
dataset$term<-as.numeric(dataset$term)
dataset$emp_length = factor(dataset$emp_length,
                                levels= c("n/a","< 1 year",
                                          "1 year","2 years",
                                          "3 years","4 years",
                                          "5 years","6 years",
                                          "7 years","8 years",
                                          "9 years","10 years", "10+ years"),
                                labels=c(0,0.5,1,2,3,4,5,6,7,8,9,10,20))
dataset$emp_length<-as.numeric(sub("%","",dataset$emp_length))/10
#remove loanstatuc
dataset$loan_status <- NULL
#dummy present (present by 0 or 1)
dataset <- dummy.data.frame(dataset, names=c("home_ownership"), sep="_")
dataset <- dummy.data.frame(dataset, names=c("verification_status"), sep="_")
dataset <- dummy.data.frame(dataset, names=c("purpose"), sep="_")
#change to %numerice
dataset$int_rate<-as.numeric(sub("%","",dataset$int_rate))/100
dataset$revol_util<-as.numeric(sub("%","",dataset$revol_util))/100
dataset$emp_length<-as.numeric(sub("%","",dataset$emp_length))/10
##check
str(dataset)
###add new label
dataset<-mutate(dataset,
                label = (dataset$funded_amnt_inv-dataset$total_pymnt_inv)/dataset$funded_amnt_inv)
###

write.csv(dataset2016,"newall2016.csv")
