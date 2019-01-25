#undersample
###very important
dataset<-select(dataset2016,c(3,4 ,5, 6,7, 10, 11, 12, 13,14 ,19 ,23 ,24, 25, 26 ,29,30 ,31 ,32, 33,38,15))
#filter loan status to remine Full pay, default, charge off
dataset<-filter(dataset,loan_status =="Fully Paid"|loan_status =="Charged Off"|loan_status =="Default")

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
z<-dataset$ITP>=1
dataset[z,"ITP"]<- 1

#creat new variable call revol to payment ratio RTI
dataset<-mutate(dataset,RTI =dataset$revol_bal/mic)
B<-dataset$RTI>=4
dataset[B,"RTI"]<- 4

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
                                labels=c(0,0.5,1,2,3,4,5,6,7,8,9,9.9,10))

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
#creat new variable call return_rate and loss_rate 

dataset<-mutate(dataset,return_rate =(1+dataset$int_rate)^(5*dataset$term))
#treasure rate in 2016/01 is about 2.09%, we use it as our risk-free interest rate
dataset<-mutate(dataset,
                    gain = (dataset$total_pymnt_inv-dataset$funded_amnt_inv)/(dataset$funded_amnt_inv*1.0209^dataset$term))

#payback rate we use return rate to calculate
dataset<-mutate(dataset,
                    payback_rate = (dataset$total_pymnt_inv)/(dataset$funded_amnt_inv*return_rate))

#replace the gain_rate is big then 1
bg<-dataset$payback_rate >=1
dataset[bg,"payback_rate"]<- 1
#change the label
loanStatus<-c("Fully Paid"=0,"Default"=1,"Charged Off"=1)
dataset$loan_status <- loanStatus[dataset$loan_status]

###########################################################################################
#normalize
dataset$pub_rec<-BBmisc::normalize(dataset$pub_rec,method="range",range=c(0:1))
dataset$installment<-BBmisc::normalize(dataset$installment,method="range",range=c(0:1))
dataset$total_acc<-BBmisc::normalize(dataset$total_acc,method="range",range=c(0:1))
dataset$delinq_2yrs<-BBmisc::normalize(dataset$delinq_2yrs,method="range",range=c(0:1))
dataset$annual_inc<-BBmisc::normalize(dataset$annual_inc,method="range",range=c(0:1))
dataset$total_acc<-BBmisc::normalize(dataset$total_acc,method="range",range=c(0:1))
dataset$funded_amnt_inv<-BBmisc::normalize(dataset$funded_amnt_inv,method="range",range=c(0:1))
dataset$installment<-BBmisc::normalize(dataset$installment,method="range",range=c(0:1))
dataset$RTI<-BBmisc::normalize(dataset$RTI,method="range",range=c(0:1))
dataset$open_acc<-BBmisc::normalize(dataset$open_acc,method="range",range=c(0:1))
dataset$dti<-BBmisc::normalize(dataset$dti,method="range",range=c(0:1))
dataset$inq_last_6mths<-BBmisc::normalize(dataset$inq_last_6mths,method="range",range=c(0:1))
dataset$dti<-BBmisc::normalize(dataset$dti,method="range",range=c(0:1))
dataset$revol_util<-BBmisc::normalize(dataset$revol_util,method="range",range=c(0:1))
dataset$credit_age<-BBmisc::normalize(as.numeric(dataset$credit_age),method="range",range=c(0:1))
dataset$revol_util<-BBmisc::normalize(dataset$revol_util,method="range",range=c(0:1))
dataset$RTI<-BBmisc::normalize(dataset$RTI,method="range",range=c(0:1))
#missing data
#replace 0 to the NaN result

dataset$dti[is.na(dataset$dti)]<-mean(dataset$dti,na.rm = TRUE)
dataset$revol_util[is.na(dataset$revol_util)]<-mean(dataset$revol_util,na.rm = TRUE)
dataset$inq_last_6mths[is.na(dataset$inq_last_6mths)]<-0
dataset<-na.omit(dataset)
#################
#for different dataset
dataset<-select(dataset,c(1,2,3,4,6,7,8,9,10,
                              11,12,13,14,15,16,
                              17,18,19,20,21,22,
                              23,24,25,26,27,28,29,30,31,32,
                              37,38,40,35,41))
cdataset<-dataset[-c(34,36)]
# minority should be class 1 
library(unbalanced)
input<-dataset[,-34]# have to change to number
Y<-dataset$loan_status
data<-ubUnder(X=input,Y= Y,perc = 50,method="percPos")
newData<-cbind(data$X,data$Y)
names(newData)<-c("funded_amnt_inv", 
                  "term", "int_rate",                           
                  "installment","emp_length",                         
                  "home_ownership_ANY","home_ownership_MORTGAGE",            
                  "home_ownership_OWN" ,"home_ownership_RENT",
                   "annual_inc", "verification_status_Not Verified",   
                   "verification_status_Source Verified","verification_status_Verified",       
                   "purpose_car", "purpose_credit_card",                
                  "purpose_debt_consolidation" ,"purpose_home_improvement"  ,         
                  "purpose_house" , "purpose_major_purchase" ,            
                  "purpose_medical" , "purpose_moving",                     
                  "purpose_other" , "purpose_renewable_energy",           
                  "purpose_small_business","purpose_vacation" ,                  
                  "dti"  , "delinq_2yrs" ,                       
                  "inq_last_6mths", "open_acc" ,                          
                  "pub_rec" ,  "revol_util" ,                        
                                       
                  "ITP" , "RTI",                                
                  "loan_status")


###
#for featureselection 
mRMR<-newData[c(31,26,32,29,15,30,27,17,19,2,34)]
LASSO<-newData[c(2,  3,  5,  7 , 9 ,10 ,11 ,13, 14, 16, 17 ,18, 19, 20 ,21 ,22, 23 ,24, 25, 26, 27 ,28, 29, 30 ,31, 32, 33,34)]
LASSO<-dataset[c(2,  3,  5,  7 , 9 ,10 ,11 ,13, 14, 16, 17 ,18, 19, 20 ,21 ,22, 23 ,24, 25, 26, 27 ,28, 29, 30 ,31, 32, 33,35)]
