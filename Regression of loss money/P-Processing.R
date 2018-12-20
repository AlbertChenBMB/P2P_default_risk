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
dataset1<-read.csv("LoanStats_2016Q1.csv",stringsAsFactors = FALSE)
#dataset1<-dataset1[1:133888,3:145]
####datase2  have problems
dataset2<-read.csv("LoanStats_2016Q2.csv",stringsAsFactors = FALSE)
#dataset2<-dataset2[1:97855,3:145]
dataset3<-read.csv("LoanStats_2016Q3.csv",stringsAsFactors = FALSE)
#dataset3<-dataset3[1:99121,3:145]
dataset4<-read.csv("LoanStats_2016Q4.csv",stringsAsFactors = FALSE)
#dataset4<-dataset4[1:103547,3:145]
#test dataset
#ver.set<-dataset4
#used only dataset4 to be test set
#str(dataset4)
#merge data without dataset4
dataset2016<-Reduce(function(x, y) merge(x, y, all=TRUE), list(dataset1, dataset2, dataset3,dataset4))

##############################################################################################################
#
tra.dataset<-select(dataset2016,c(3,4 ,5, 6,7, 10, 11, 12, 13,14 ,19 ,23 ,24, 25, 26 ,29,30 ,31 ,32, 33,38,15))
#filter loan status to remine Full pay, default, charge off
tra.dataset<-filter(tra.dataset,loan_status =="Fully Paid"|loan_status =="Charged Off"|loan_status =="Default")

str(tra.dataset)
tbl_df(tra.dataset)
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
 
#creat new variable call revol to payment ratio RTI
tra.dataset<-mutate(tra.dataset,RTI =tra.dataset$revol_bal/mic)
B<-tra.dataset$RTI>=4
tra.dataset[B,"RTI"]<- 4

#delet old variable
tra.dataset$mic<-NULL
tra.dataset$revol_bal<-NULL
##################################
##Encoding the target feature as factor
tra.dataset$emp_length = factor(tra.dataset$emp_length,
                                levels= c("n/a","< 1 year",
                                          "1 year","2 years",
                                          "3 years","4 years",
                                          "5 years","6 years",
                                          "7 years","8 years",
                                          "9 years","10 years", "10+ years"),
                                labels=c(0,0.5,1,2,3,4,5,6,7,8,9,9.9,10))

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
tra.dataset<-mutate(tra.dataset,return_rate =(1+tra.dataset$int_rate)^(5*tra.dataset$term))
#treasure rate in 2016/01 is about 2.09%, we use it as our risk-free interest rate
tra.dataset<-mutate(tra.dataset,
                    gain = (tra.dataset$total_pymnt_inv-tra.dataset$funded_amnt_inv)/(tra.dataset$funded_amnt_inv*1.0209^tra.dataset$term))

#payback rate we use return rate to calculate
tra.dataset<-mutate(tra.dataset,
                    payback_rate = (tra.dataset$total_pymnt_inv)/(tra.dataset$funded_amnt_inv*return_rate))

#replace the gain_rate is big then 1
bg<-tra.dataset$payback_rate >=1
tra.dataset[bg,"payback_rate"]<- 1
#change the label
loanStatus<-c("Fully Paid"=1,"Default"=0,"Charged Off"=0)
tra.dataset$loan_status <- loanStatus[tra.dataset$loan_status]
#add label 3

#tra.dataset$loss_rate<-scale(tra.dataset$loss_rate)
#check the dataset
summary(tra.dataset)
RF_data<-tra.dataset
### export this dataset for randomforest

###########################################################################################
#normalize
tra.dataset$pub_rec<-normalize(tra.dataset$pub_rec,method="range",range=c(0:1))
tra.dataset$installment<-normalize(tra.dataset$installment,method="range",range=c(0:1))
tra.dataset$total_acc<-normalize(tra.dataset$total_acc,method="range",range=c(0:1))
tra.dataset$delinq_2yrs<-normalize(tra.dataset$delinq_2yrs,method="range",range=c(0:1))
tra.dataset$annual_inc<-normalize(tra.dataset$annual_inc,method="range",range=c(0:1))
tra.dataset$total_acc<-normalize(tra.dataset$total_acc,method="range",range=c(0:1))
tra.dataset$funded_amnt_inv<-normalize(tra.dataset$funded_amnt_inv,method="range",range=c(0:1))
tra.dataset$installment<-normalize(tra.dataset$installment,method="range",range=c(0:1))
tra.dataset$RTI<-normalize(tra.dataset$RTI,method="range",range=c(0:1))
tra.dataset$open_acc<-normalize(tra.dataset$open_acc,method="range",range=c(0:1))
tra.dataset$dti<-normalize(tra.dataset$dti,method="range",range=c(0:1))
tra.dataset$inq_last_6mths<-normalize(tra.dataset$inq_last_6mths,method="range",range=c(0:1))
tra.dataset$dti<-normalize(tra.dataset$dti,method="range",range=c(0:1))
tra.dataset$revol_util<-normalize(tra.dataset$revol_util,method="range",range=c(0:1))
tra.dataset$credit_age<-normalize(as.numeric(tra.dataset$credit_age),method="range",range=c(0:1))
tra.dataset$revol_util<-normalize(tra.dataset$revol_util,method="range",range=c(0:1))
tra.dataset$RTI<-normalize(tra.dataset$RTI,method="range",range=c(0:1))
#missing data
#replace 0 to the NaN result

tra.dataset$dti[is.na(tra.dataset$dti)]<-mean(tra.dataset$dti,na.rm = TRUE)
tra.dataset$revol_util[is.na(tra.dataset$revol_util)]<-mean(tra.dataset$revol_util,na.rm = TRUE)
tra.dataset$inq_last_6mths[is.na(tra.dataset$inq_last_6mths)]<-0
na.omit(tra.dataset)
##check
summary(tra.dataset)
#information
mean(tra.dataset$payback_rate)
mean(tra.dataset$gain)
quantile(tra.dataset$payback_rate)
quantile(tra.dataset$gain)
hist(tra.dataset$gain,main = "2016 profit distribution",xlab = "Gain")
hist(tra.dataset$payback_rate,main = "2016 payback distribution",xlab = "Pay back rate")
#################
#for different dataset


str(tra.dataset)
dataset<-select(tra.dataset,c(1,2,3,4,6,7,8,9,10,
                              11,12,13,14,15,16,
                              17,18,19,20,21,22,
                              23,24,25,26,27,28,29,30,31,32,
                              38,40,35,41))

#training dataset
#for classification 
#1. all features <- C_train & testset
#2. mRMR         <- m_C_train & m_testset

#for regression 
#1. all features <- L_train & testset
#2. mRMR         <- m_L_train & m_testset

library(caTools)
set.seed(456605)
split = sample.split(dataset, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
testset = subset(dataset, split == FALSE)
L_train<-training_set[-c(33,34)]
C_train<-training_set[-c(33,35)]
dataset<-na.omit(dataset)

#dataset with mRMR
m_dataset<-select(dataset,c(3,9,20,26,27,24,6,14,28,23,33,34,35))
set.seed(37)
split = sample.split(m_dataset, SplitRatio = 0.70)
m_training_set = subset(m_dataset, split == TRUE)
m_testset = subset(m_dataset, split == FALSE)

m_L_train<-m_training_set[-c(11,12)]
m_C_train<-m_training_set[-c(11,13)]

#export dataset
write.csv(m_L_train,"m_L_train.csv")
write.csv(m_testset,"m_test.csv")

