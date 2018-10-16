library(dplyr)
library(data.table)
library(dummies)
#for time
library(lubridate)
library(e1071)
#read data for 2016
dataset1<-read.csv("LoanStats_2016Q1.csv",stringsAsFactors = FALSE)
dataset2<-read.csv("LoanStats_2016Q2.csv",stringsAsFactors = FALSE)
dataset3<-read.csv("LoanStats_2016Q3.csv",stringsAsFactors = FALSE)
dataset4<-read.csv("LoanStats_2016Q4.csv",stringsAsFactors = FALSE)
# #read data for 2017
# dataset1<-read.csv("LoanStats_2017Q1.csv",stringsAsFactors = FALSE)
# dataset7_2<-read.csv("LoanStats_2017Q2.csv",stringsAsFactors = FALSE)
# dataset7_3<-read.csv("LoanStats_2017Q3.csv",stringsAsFactors = FALSE)
# dataset7_4<-read.csv("LoanStats_2017Q4.csv",stringsAsFactors = FALSE)

#merge data
all_dataset2016<-Reduce(function(x, y) merge(x, y, all=TRUE), list(dataset1, dataset2, dataset3,dataset4))
###
#############################################################################################################
#select necessary variables and creat the new dataset
dataset2016<-select(all_dataset2016,c(1, 3,4 ,5, 6, 10, 11, 12, 13,14 ,19 ,23 ,24, 25, 26 ,29,30 ,31 ,32, 33,15))
#filter loan status to remine Full pay, default, charge off
dataset2016<-filter(dataset2016,loan_status =="Fully Paid"|loan_status =="Charged Off"|loan_status =="Default")
str(dataset2016)
tbl_df(dataset2016)
###
############################################################################################################
#Data pre-processing
# Encoding the target feature as factor
dataset2016$term = factor(dataset2016$term,
                          levels = c(" 36 months"," 60 months"),
                          labels = c(0.6, 1))
# #time problem rember change in csv
dataset2016$issue_d<-dmy(paste("01-", dataset2016$issue_d , sep =""))
dataset2016$earliest_cr_line<-dmy(paste("01-", dataset2016$earliest_cr_line , sep =""))
#creat new variable call credit age
dataset2016<-mutate(dataset2016,
                    credit_age = difftime(dataset2016$issue_d,dataset2016$earliest_cr_line,units="weeks"))

#creat new variable call income to payment ratio ITP
dataset2016<-mutate(dataset2016,mic = dataset2016$annual_inc/12,ITP = (dataset2016$installment/mic))
# scale it 

#creat new variable call income to payment ratio RTI
dataset2016<-mutate(dataset2016,RTI =dataset2016$revol_bal/mic)

#delet old variable
dataset2016$mic<-NULL
dataset2016$revol_bal<-NULL

dataset2016$term<-as.numeric(dataset2016$term)
dataset2016$emp_length = factor(dataset2016$emp_length,
                                levels= c("n/a","< 1 year",
                                          "1 year","2 years",
                                          "3 years","4 years",
                                          "5 years","6 years",
                                          "7 years","8 years",
                                          "9 years","10 years", "10+ years"),
                                labels=c(0,0.5,1,2,3,4,5,6,7,8,9,10,20))
dataset2016$emp_length<-as.numeric(sub("%","",dataset2016$emp_length))/10
#remove some variables
dataset2016$issue_d<-NULL
dataset2016$earliest_cr_line<-NULL
#dummy present (present by 0 or 1)
dataset2016 <- dummy.data.frame(dataset2016, names=c("home_ownership"), sep="_")
dataset2016 <- dummy.data.frame(dataset2016, names=c("verification_status"), sep="_")
dataset2016 <- dummy.data.frame(dataset2016, names=c("purpose"), sep="_")
dataset2016$grade<-NULL
dataset2016$sub_grade<-NULL
#change to %numerice
dataset2016$int_rate<-as.numeric(sub("%","",dataset2016$int_rate))/100
dataset2016$revol_util<-as.numeric(sub("%","",dataset2016$revol_util))/100
dataset2016$emp_length<-as.numeric(sub("%","",dataset2016$emp_length))/10
#change the label
loanStatus<-c("Fully Paid"=0,"Default"=1,"Charged Off"=1)
dataset2016$loan_status <- loanStatus[dataset2016$loan_status]
#check the dataset
summary(dataset2016)
###
###########################################################################################
#scale to dataset
dataset2016$credit_age<-log(as.numeric(dataset2016$credit_age))
#sigmoid
dataset2016$ITP<-sigmoid(dataset2016$ITP)
dataset2016$RTI<-sigmoid(dataset2016$RTI)
dataset2016$pub_rec<-sigmoid(dataset2016$pub_rec)
#normalize to annual income, total amount
dataset2016$installment<-log(dataset2016$installment)
dataset2016$loan_amnt<-log(dataset2016$loan_amnt)
dataset2016$funded_amnt_inv<-log(dataset2016$funded_amnt_inv)
dataset2016$total_acc<-log(dataset2016$total_acc)
#scale some
dataset2016$annual_inc<-scale(dataset2016$annual_inc)
dataset2016$total_acc<-scale(dataset2016$total_acc)
#missing data
#replace 0 to the NaN result
dataset2016$credit_age[is.na(dataset2016$credit_age)]<-0
dataset2016$dti[is.na(dataset2016$dti)]<-mean(dataset2016$dti,na.rm = TRUE)
dataset2016$revol_util[is.na(dataset2016$revol_util)]<-mean(dataset2016$revol_util,na.rm = TRUE)
##check
summary(dataset2016)
###
dataset2016<-na.omit(dataset2016)
write.csv(dataset2016,"newall2016.csv")

