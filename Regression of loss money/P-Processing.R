library(dplyr)
library(data.table)
library(dummies)
library(e1071)
#for normalize
library(BBmisc)
#for time
library(lubridate)
#read data for 2016
#######################################################
#
# Compare two things:
#         first: classification
#         second: LR grade
########################################################
dataset1<-read.csv("LoanStats_2016Q1.csv",stringsAsFactors = FALSE,skip = 1)
dataset1<-dataset1[1:133888,3:145]
####datase2  have problems
dataset2<-read.csv("LoanStats_2016Q2.csv",stringsAsFactors = FALSE,skip = 1)
dataset2<-dataset2[1:97855,3:145]
dataset3<-read.csv("LoanStats_2016Q3.csv",stringsAsFactors = FALSE,skip = 1)
dataset3<-dataset3[1:99121,3:145]
dataset4<-read.csv("LoanStats_2016Q4.csv",stringsAsFactors = FALSE,skip = 1)
dataset4<-dataset4[1:103547,3:145]
#test dataset
ver.set<-dataset4
#used only dataset4 to be test set
str(dataset4)
#merge data without dataset4
dataset<-Reduce(function(x, y) merge(x, y, all=TRUE), list(dataset1, dataset2, dataset3))
##############################################################################################################


#try only dataset4 first
#select necessary variables
ver.set<-select(dataset4,c(3,4 ,5, 6,7, 10, 11, 12, 13,14 ,19 ,23 ,24, 25, 26 ,29,30 ,31 ,32, 33,38,15))
tra.dataset<-select(dataset,c(3,4 ,5, 6,7, 10, 11, 12, 13,14 ,19 ,23 ,24, 25, 26 ,29,30 ,31 ,32, 33,38,15))
#filter loan status to remine Full pay, default, charge off
tra.dataset<-filter(tra.dataset,loan_status =="Fully Paid"|loan_status =="Charged Off"|loan_status =="Default")

str(dataset)
tbl_df(dataset)
########################################################################################################
#Data pre-processing

##time problem rember change in csv about term
tra.dataset$term = factor(tra.dataset$term,
                          levels = c(" 36 months"," 60 months"),
                          labels = c(0.6, 1))
tra.dataset$term<-as.numeric(levels(tra.dataset$term))[tra.dataset$term]

##time problem rember change in csv about credit age
tra.dataset$issue_d<-dmy(paste("01-", tra.dataset$issue_d , sep =""))
tra.dataset$earliest_cr_line<-dmy(paste("01-", tra.dataset$earliest_cr_line , sep =""))
#creat new variable call credit age
tra.dataset<-mutate(tra.dataset,
                    credit_age = difftime(tra.dataset$issue_d,tra.dataset$earliest_cr_line,units="weeks"))
######################################################################################################
#Add new variable
#creat new variable call income to payment ratio ITP
tra.dataset<-mutate(tra.dataset,mic = tra.dataset$annual_inc/12,ITP = (tra.dataset$installment/mic))
z<-tra.dataset$ITP>=1
tra.dataset[z,"ITP"]<- 1
# scale it 
#creat new variable call revol to payment ratio RTI
tra.dataset<-mutate(tra.dataset,RTI =tra.dataset$revol_bal/mic)
B<-tra.dataset$RTI>=4
tra.dataset[B,"RTI"]<- 4
tra.dataset$RTI<-normalize(tra.dataset$RTI)
#delet old variable
dataset$mic<-NULL
dataset$revol_bal<-NULL
##################################
##Encoding the target feature as factor
tra.dataset$emp_length = factor(tra.dataset$emp_length,
                                levels= c("n/a","< 1 year",
                                          "1 year","2 years",
                                          "3 years","4 years",
                                          "5 years","6 years",
                                          "7 years","8 years",
                                          "9 years","10 years", "10+ years"),
                                labels=c(0,0.5,1,2,3,4,5,6,7,8,9,10,20))

#remove some variables
tra.dataset$issue_d<-NULL
tra.dataset$earliest_cr_line<-NULL
#dummy present (present by 0 or 1)
tra.dataset <- dummy.data.frame(tra.dataset, names=c("home_ownership"), sep="_")
tra.dataset <- dummy.data.frame(tra.dataset, names=c("verification_status"), sep="_")
tra.dataset <- dummy.data.frame(tra.dataset, names=c("purpose"), sep="_")

#change to %numerice
tra.dataset$int_rate<-as.numeric(sub("%","",tra.dataset$int_rate))/100
tra.dataset$revol_util<-as.numeric(sub("%","",tra.dataset$revol_util))/100
tra.dataset$emp_length<-as.numeric(sub("%","",tra.dataset$emp_length))/10
#creat new variable call return_rate and loss_rate 
tra.dataset<-mutate(tra.dataset,return_rate =(1+tra.dataset$int_rate)^(2*tra.dataset$term))
#treasure rate in 2016/01 is about 2.09%, we use it as our risk-free interest rate
tra.dataset<-mutate(tra.dataset,
                    loss_rate = log((tra.dataset$total_pymnt_inv)/(tra.dataset$funded_amnt_inv*1.0209)))
#replace the loss_rate is small then -1
inf<-tra.dataset$loss_rate<=-1
tra.dataset[inf,"loss_rate"]<- -1
#replace the loss_rate is big then 1
pos<-tra.dataset$loss_rate>=1
tra.dataset[pos,"loss_rate"]<- 1
#change the label
loanStatus<-c("Fully Paid"=0,"Default"=1,"Charged Off"=1)
tra.dataset$loan_status <- loanStatus[tra.dataset$loan_status]
tra.dataset$loss_rate<-scale(tra.dataset$loss_rate)
#check the dataset
summary(tra.dataset)
###
###########################################################################################
#problem: some of data range is too big, can't find suitfor method to scale it
#scale some data
tra.dataset$pub_rec<-(tra.dataset$pub_rec)/61
tra.dataset$total_acc<-(tra.dataset$total_acc)/176
tra.dataset$delinq_2yrs<-(tra.dataset$total_acc)/21


tra.dataset$annual_inc<-scale(tra.dataset$annual_inc)
tra.dataset$total_acc<-scale(tra.dataset$total_acc)
tra.dataset$funded_amnt_inv<-scale(tra.dataset$funded_amnt_inv)
tra.dataset$installment<-scale(tra.dataset$installment)
#normalize to annual income, total amount
tra.dataset$credit_age<-log(as.numeric(tra.dataset$credit_age))
tra.dataset$credit_age[is.na(tra.dataset$credit_age)]<-0

#missing data
#replace 0 to the NaN result

tra.dataset$dti[is.na(tra.dataset$dti)]<-mean(tra.dataset$dti,na.rm = TRUE)
tra.dataset$revol_util[is.na(tra.dataset$revol_util)]<-mean(tra.dataset$revol_util,na.rm = TRUE)
tra.dataset$inq_last_6mths[is.na(tra.dataset$inq_last_6mths)]<-0
na.omit(tra.dataset)
##check
summary(tra.dataset)


