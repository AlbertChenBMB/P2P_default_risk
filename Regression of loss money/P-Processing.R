setwd("C:/Users/SF/Desktop/P2P_final/dataset")
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
#read data and skip first 2 row becaue they are describe line and because header is true so skip =1)
Q12016<-read.csv(("LoanStats_2016Q1.csv"),header = TRUE,sep = ",",skip = 1,stringsAsFactors = FALSE)
Q22016<-read.csv(("LoanStats_2016Q2.csv"),header = TRUE,sep = ",",skip = 1,stringsAsFactors = FALSE)
Q32016<-read.csv(("LoanStats_2016Q3.csv"),header = TRUE,sep = ",",skip = 1,stringsAsFactors = FALSE)
Q42016<-read.csv(("LoanStats_2016Q4.csv"),header = TRUE,sep = ",",skip = 1,stringsAsFactors = FALSE)
Q12017<-read.csv(("LoanStats_2017Q1.csv"),header = TRUE,sep = ",",skip = 1,stringsAsFactors = FALSE)
Q22017<-read.csv(("LoanStats_2017Q2.csv"),header = TRUE,sep = ",",skip = 1,stringsAsFactors = FALSE)
Q32017<-read.csv(("LoanStats_2017Q3.csv"),header = TRUE,sep = ",",skip = 1,stringsAsFactors = FALSE)
Q42017<-read.csv(("LoanStats_2017Q4.csv"),header = TRUE,sep = ",",skip = 1,stringsAsFactors = FALSE)
Q12018<-read.csv(("LoanStats_2016Q1.csv"),header = TRUE,sep = ",",skip = 1,stringsAsFactors = FALSE)

#dataset1<-read.csv("LoanStats_2016Q1.csv",stringsAsFactors = FALSE)
dataset1<-Q12016[1:133887,3:145]
####datase2  have problems
#dataset2<-read.csv("LoanStats_2016Q2.csv",stringsAsFactors = FALSE)
dataset2<-Q22016[1:97854,3:145]
#dataset3<-read.csv("LoanStats_2016Q3.csv",stringsAsFactors = FALSE)
dataset3<-Q32016[1:99120,3:145]
#dataset4<-read.csv("LoanStats_2016Q4.csv",stringsAsFactors = FALSE)
dataset4<-Q42016[1:103546,3:145]
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
rm(z) 
#creat new variable call revol to payment ratio RTI
tra.dataset<-mutate(tra.dataset,RTI =tra.dataset$revol_bal/mic)
B<-tra.dataset$RTI>=4
tra.dataset[B,"RTI"]<- 4
rm(B)
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

tra.dataset<-mutate(tra.dataset,expect_return_rate =(1+tra.dataset$int_rate)^(5*tra.dataset$term))
#treasure rate in 2016/01 is about 2.09%, we use it as our risk-free interest rate
tra.dataset<-mutate(tra.dataset,
                    ROI = (1+((tra.dataset$total_pymnt_inv-tra.dataset$funded_amnt_inv)/(tra.dataset$funded_amnt_inv)))^(1/(5*tra.dataset$term)))
summary(tra.dataset$ROI)
#Net return rate we use return rate to calculate
tra.dataset<-mutate(tra.dataset,
                    NRR = (tra.dataset$total_pymnt_inv)/(tra.dataset$funded_amnt_inv*expect_return_rate))

#replace the gain_rate is big then 1
bg<-tra.dataset$NRR >=1
tra.dataset[bg,"NRR"]<- 1
rm(bg)
#change the label
loanStatus<-c("Fully Paid"=1,"Default"=0,"Charged Off"=0)
tra.dataset$loan_status <- loanStatus[tra.dataset$loan_status]
summary(tra.dataset$loan_status)

#check the dataset
#add new label profit
tra.dataset<-mutate(tra.dataset,
                    profit = tra.dataset$funded_amnt_inv-tra.dataset$total_pymnt_inv)
###########################################################################################
#normalize
tra.dataset$pub_rec<-BBmisc::normalize(tra.dataset$pub_rec,method="range",range=c(0:1))
tra.dataset$installment<-BBmisc::normalize(tra.dataset$installment,method="range",range=c(0:1))
tra.dataset$total_acc<-BBmisc::normalize(tra.dataset$total_acc,method="range",range=c(0:1))
tra.dataset$delinq_2yrs<-BBmisc::normalize(tra.dataset$delinq_2yrs,method="range",range=c(0:1))
tra.dataset$annual_inc<-BBmisc::normalize(tra.dataset$annual_inc,method="range",range=c(0:1))
tra.dataset$total_acc<-BBmisc::normalize(tra.dataset$total_acc,method="range",range=c(0:1))
tra.dataset$funded_amnt_inv<-BBmisc::normalize(tra.dataset$funded_amnt_inv,method="range",range=c(0:1))
tra.dataset$installment<-BBmisc::normalize(tra.dataset$installment,method="range",range=c(0:1))
tra.dataset$RTI<-BBmisc::normalize(tra.dataset$RTI,method="range",range=c(0:1))
tra.dataset$open_acc<-BBmisc::normalize(tra.dataset$open_acc,method="range",range=c(0:1))
tra.dataset$dti<-BBmisc::normalize(tra.dataset$dti,method="range",range=c(0:1))
tra.dataset$inq_last_6mths<-BBmisc::normalize(tra.dataset$inq_last_6mths,method="range",range=c(0:1))
tra.dataset$dti<-BBmisc::normalize(tra.dataset$dti,method="range",range=c(0:1))
tra.dataset$revol_util<-BBmisc::normalize(tra.dataset$revol_util,method="range",range=c(0:1))
tra.dataset$credit_age<-BBmisc::normalize(as.numeric(tra.dataset$credit_age),method="range",range=c(0:1))
tra.dataset$revol_util<-BBmisc::normalize(tra.dataset$revol_util,method="range",range=c(0:1))
tra.dataset$RTI<-BBmisc::normalize(tra.dataset$RTI,method="range",range=c(0:1))
#missing data
#replace 0 to the NaN result

tra.dataset$dti[is.na(tra.dataset$dti)]<-mean(tra.dataset$dti,na.rm = TRUE)
tra.dataset$revol_util[is.na(tra.dataset$revol_util)]<-mean(tra.dataset$revol_util,na.rm = TRUE)
tra.dataset$inq_last_6mths[is.na(tra.dataset$inq_last_6mths)]<-0
tra.dataset<-na.omit(tra.dataset)
##check
summary(tra.dataset)
#information
mean(tra.dataset$NRR)
mean(tra.dataset$ROI)
quantile(tra.dataset$NRR)
quantile(tra.dataset$profit)
hist(tra.dataset$ROI,main = "2016 profit distribution",xlab = "ROI")
hist(tra.dataset$NRR,main = "2016 payback distribution",xlab = "NRR")
#################
#for different dataset


str(tra.dataset)
dataset<-select(tra.dataset,c(1,2,3,4,6,7,8,9,10,
                              11,12,13,14,15,16,
                              17,18,19,20,21,22,
                              23,24,25,26,27,28,29,30,31,32,33,
                              37,38,39,41,42,36))
rm(dataset1)
rm(dataset2)
rm(dataset3)
rm(dataset4)
#training dataset
#for classification 
#1. all features <- C_train & testset
#2. mRMR         <- m_C_train & m_testset

#for regression 
#1. all features <- L_train & testset
#2. mRMR         <- m_L_train & m_testset
#################
names(dataset)[11]<-"verification_status_Not_Verified"
names(dataset)[12]<-"verification_status_Source_Verified"

library(caTools)
set.seed(3456345)
split = sample.split(dataset, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
testset = subset(dataset, split == FALSE)

mean(training_set$NRR)
mean(testset$NRR)
L_train<-training_set[-c(36,38)]
C_train<-training_set[-c(36,37)]
# names(testset)[12]<-"verification_status_Source_Verified"
# names(L_train)[12]<-"verification_status_Source_Verified"
# names(L_train)[11]<-"verification_status_Not_Verified"
# names(testset)[11]<-"verification_status_Not_Verified"
# names(C_train)[12]<-"verification_status_Source_Verified"
# names(C_train)[11]<-"verification_status_Not_Verified"
# names(training_set)[12]<-"verification_status_Source_Verified"
# names(training_set)[11]<-"verification_status_Not_Verified"
################################################################
#for new feature selection
#for featureselection 
mRMR<-training_set[mRMRresut]
m_test<-testset[c(31,26,32,29,15,30,27,17,19,2,37,38,39)]



#export dataset
write.csv(m_L_train,"m_L_train.csv")
write.csv(m_testset,"m_test.csv")



