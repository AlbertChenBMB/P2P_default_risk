#undersample

library(unbalanced)
input<-dataset2016[,-34]# have to change to number
Y<-dataset2016$loan_status
data<-ubUnder(X=input,Y= Y,perc = 50,method="percPos")
newData<-cbind(data$X,data$Y)
names(newData)<-c("loan_amnt" , "funded_amnt_inv", 
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
                  "total_acc" , "credit_age" ,                        
                  "ITP" , "RTI",                                
                  "label")


###
#for featureselection 
mRMR<-newData[c(4,10,21,27,28,25,7,15,29,24,37)]
LASSO<-newData[c(1,3,4,6,8,10,11,12,14,15,17,18,19,20,21,22,23,25,26,27,28, 29,30,31,32,33,34,35,36,37)]
