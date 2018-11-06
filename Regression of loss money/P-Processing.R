library(dplyr)
library(data.table)
library(dummies)
library(e1071)
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
tra.dataset<-Reduce(function(x, y) merge(x, y, all=TRUE), list(dataset1, dataset2, dataset3))
##############################################################################################################


#try only dataset4 first
#select necessary variables
dataset<-select(dataset4,c(1, 3,4 ,5, 6,7, 10, 11, 12, 13,14 ,19 ,23 ,24, 25, 26 ,29,30 ,31 ,32, 33,38,15))
#filter loan status to remine Full pay, default, charge off
dataset<-filter(dataset,loan_status =="Fully Paid"|loan_status =="Charged Off"|loan_status =="Default")
str(dataset)
tbl_df(dataset)
########################################################################################################
#Data pre-processing

##time problem rember change in csv about term
dataset$term = factor(dataset$term,
                          levels = c(" 36 months"," 60 months"),
                          labels = c(0.6, 1))
dataset$term<-as.numeric(levels(dataset$term))[dataset$term]

##time problem rember change in csv about credit age
dataset$issue_d<-dmy(paste("01-", dataset$issue_d , sep =""))
dataset$earliest_cr_line<-dmy(paste("01-", dataset$earliest_cr_line , sep =""))
#creat new variable call credit age
dataset<-mutate(dataset,
                    credit_age = difftime(dataset$issue_d,dataset$earliest_cr_line,units="weeks"))
######################################################################################################
#Add new variable
#creat new variable call income to payment ratio ITP
dataset<-mutate(dataset,mic = dataset$annual_inc/12,ITP = (dataset$installment/mic))
# scale it 
#creat new variable call income to payment ratio RTI
dataset<-mutate(dataset,RTI =dataset$revol_bal/mic)

#delet old variable
dataset$mic<-NULL
dataset$revol_bal<-NULL
##################################
##Encoding the target feature as factor
dataset$emp_length = factor(dataset$emp_length,
                                levels= c("n/a","< 1 year",
                                          "1 year","2 years",
                                          "3 years","4 years",
                                          "5 years","6 years",
                                          "7 years","8 years",
                                          "9 years","10 years", "10+ years"),
                                labels=c(0,0.5,1,2,3,4,5,6,7,8,9,10,20))
dataset$emp_length<-as.numeric(sub("%","",dataset$emp_length))/10

#remove some variables
dataset$issue_d<-NULL
dataset$earliest_cr_line<-NULL
#dummy present (present by 0 or 1)
dataset <- dummy.data.frame(dataset, names=c("home_ownership"), sep="_")
dataset <- dummy.data.frame(dataset, names=c("verification_status"), sep="_")
dataset <- dummy.data.frame(dataset, names=c("purpose"), sep="_")

#change to %numerice
dataset$int_rate<-as.numeric(sub("%","",dataset$int_rate))/100
dataset$revol_util<-as.numeric(sub("%","",dataset$revol_util))/100
dataset$emp_length<-as.numeric(sub("%","",dataset$emp_length))/10
#creat new variable call return_rate and new label
dataset<-mutate(dataset,return_rate =(1+dataset$int_rate)^(2*dataset$term))
dataset<-mutate(dataset,
                label = ((dataset$funded_amnt_inv*dataset$return_rate)-dataset$total_pymnt_inv)/(dataset$funded_amnt_inv*dataset$return_rate))
#replace the loss_rate is small then -1
inf<-dataset$loss_rate<=-1
dataset[inf,"loss_rate"]<- -1
#change the label
loanStatus<-c("Fully Paid"=0,"Default"=1,"Charged Off"=1)
dataset$loan_status <- loanStatus[dataset$loan_status]
#check the dataset
summary(dataset)
###
###########################################################################################
#scale to dataset
dataset$credit_age<-log(as.numeric(dataset$credit_age))
#sigmoid
dataset$ITP<-sigmoid(dataset$ITP)
dataset$RTI<-sigmoid(dataset$RTI)
dataset$pub_rec<-sigmoid(dataset$pub_rec)
#normalize to annual income, total amount
dataset$installment<-log(dataset$installment)
dataset$loan_amnt<-log(dataset$loan_amnt)
dataset$funded_amnt_inv<-log(dataset$funded_amnt_inv)
dataset$total_acc<-log(dataset$total_acc)
#scale some
dataset$annual_inc<-scale(dataset$annual_inc)
dataset$total_acc<-scale(dataset$total_acc)
#missing data
#replace 0 to the NaN result
dataset$credit_age[is.na(dataset$credit_age)]<-0
dataset$dti[is.na(dataset$dti)]<-mean(dataset$dti,na.rm = TRUE)
dataset$RTI[is.na(dataset$RTI)]<-mean(dataset$RTI,na.rm = TRUE)
dataset$revol_util[is.na(dataset$revol_util)]<-mean(dataset$revol_util,na.rm = TRUE)
##check
summary(dataset)

######################
#plot grade and loss, classification and loss
bwplot(x=label ~grade,data=dataset)
group_by(dataset,loan_status) %>% summarise(meanloss=mean(label))
loss<-group_by(dataset,grade) %>% summarise(meanloss=mean(label))

