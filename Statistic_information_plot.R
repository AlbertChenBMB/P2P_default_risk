library(gmodels)
CrossTable(dataset1$purpose,dataset1$loan_status,prop.r = TRUE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
CrossTable(dataset2$purpose,dataset1$loan_status,prop.r = TRUE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
CrossTable(dataset3$purpose,dataset1$loan_status,prop.r = TRUE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
CrossTable(dataset4$purpose,dataset1$loan_status,prop.r = TRUE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
#data plot
#merge data without dataset4
dataset2016<-Reduce(function(x, y) merge(x, y, all=TRUE), list(dataset1, dataset2, dataset3,dataset4))
#

PD<-select(dataset2016,c(1, 3,4 ,5, 6,7, 10, 11, 12, 13,14 ,19 ,23 ,24, 25, 26 ,29,30 ,31 ,32, 33,38,15))
#filter loan status to remine Full pay, default, charge off
PD<-filter(PD,loan_status =="Fully Paid"|loan_status =="Charged Off"|loan_status =="Default")
str(PD)
tbl_df(PD)
PD<-na.omit(PD)
########
##time problem rember change in csv about credit age1``
PD$issue_d<-dmy(paste("01-", PD$issue_d , sep =""))
PD$earliest_cr_line<-dmy(paste("01-", PD$earliest_cr_line , sep =""))
#creat new variable call credit age
PD<-mutate(PD,
                credit_age = difftime(PD$issue_d,PD$earliest_cr_line,units="weeks"))
##time problem rember change in csv about term
# PD$term = factor(PD$term,
#                       levels = c(" 36 months"," 60 months"),
#                       labels = c(0.6, 1))
# #
# PD$term<-as.numeric(levels(PD$term))[PD$term]
#creat new variable call return_rate and new label
#change to %numerice
PD$int_rate<-as.numeric(sub("%","",PD$int_rate))/100
# PD<-mutate(PD,return_rate =(1+PD$int_rate)^(2*PD$term))
# PD<-mutate(PD,
#               loss_rate = log(PD$total_pymnt_inv/(PD$funded_amnt_inv*PD$return_rate)))
# #replace the loss_rate is small then -1
# inf<-PD$loss_rate<=-1
# PD[inf,25]<- -1
PD<-mutate(PD,mic = PD$annual_inc/12,ITP = (PD$installment/mic))
z<-PD$ITP>=1
PD[z,"ITP"]<- 1
#round DTI
z<- PD$dti >= 100
PD[z,"DTI"]<-100
##########################################################
PD = PD %>%
  mutate(loan_outcome = ifelse(loan_status %in% c('Charged Off' , 'Default') , 
                               1, 
                               ifelse(loan_status == 'Fully Paid' , 0 , 'No info')
  ))
##########################################################



library(ggplot2)
#credit age VS loss_rate
plot(x =PD$credit_age,y = PD$loss_rate )
#loan_status VS loss_rate
ggplot( PD,aes(x=loan_status,y=loss_rate,fill=loan_status))+geom_point(aes(color=grade,alpha=0.3))
###grade VS loss_rate
#calculate mean of loss_rate


#calculate mean of loss for each grade
PD %>% group_by(grade) %>%summarise(mean(loss_rate,na.rm = TRUE))
PD %>% group_by(grade) %>%summarise(sd(loss_rate,na.rm = TRUE))

#plot boxplot



#with home ownership
ggplot( PD,aes(x=loan_status,y=dti,fill=home_ownership))+geom_boxplot()+coord_cartesian(ylim = c(0:100))
ggplot( PD,aes(x=loan_status,y=ITP,fill=home_ownership))+geom_boxplot()
#grade vs loss rate
ggplot( PD,aes(x=grade,y=loss_rate,fill=grade))+geom_boxplot()
#term VS loss_rate
PD$term<-as.factor(PD$term)
ggplot( PD,aes(x=term,y=loss_rate,fill=term))+geom_boxplot()




####################################################################################################################################
png(filename = "Hrelationship.png",width = 480,height = 480)
#loan_status VS home_ownership                                                                                                   ###
ggplot(PD , aes(x = home_ownership , y = ..count.. , fill = factor(loan_outcome , c(1 , 0) , c('Default' , 'Fully Paid')))) +    ###
  geom_bar() +                                                                                                                   ###
  theme(legend.title = element_blank())                                                                                          ###
# for mortga, default=19871, fullpay=72880, 21.4%                                                                                ###
# for own, default=5925, fullpay=16831, 26%
# for RENT, default=22607, fullpay=49025, 31.6%
# any<-filter(PD,home_ownership=="ANY")
# for any, default=8, fullpay=24, 26%
dev.off()
####################################################################################################################################
png(filename = "Hrelationship.png",width = 590,height = 430)
#loan_status VS purpose
ggplot(PD , aes(x = purpose , y = ..count.. , fill = factor(loan_outcome , c(1 , 0) , c('Default' , 'Fully Paid')))) + 
        geom_bar() + 
        theme(legend.title = element_blank())
dev.off()


#DTI VS grade, loss_rate
par(mfrow=c(2,1))
ggplot(PD,aes(x=dti,y=loss_rate))+geom_point(aes(color=term),alpha=0.3)+coord_cartesian(xlim = c(0,80))
ggplot(PD,aes(x=dti,y=loss_rate))+geom_point(aes(color=term),alpha=0.3)+coord_cartesian(xlim = c(80,500))
#revol
ggplot(PD,aes(x=return_rate,y=loss_rate))+geom_point(aes(color=grade,alpha=0.3))
##################################################################################################################################
#Relationship wiht  IR DTI ITP and loan_status
#plot


#plot 
png(filename = "IDrelationship.png",width = 480,height = 480)
ggplot( PD,aes(x=int_rate ,y=dti,fill=loan_outcome))+
        geom_point(aes(shape=loan_outcome,color=loan_outcome,alpha=0.2),size=2)+
        xlab("Interest Rate")+ylab("dti")+coord_cartesian(ylim = c(0:100))
dev.off()
png(filename = "IIrelationship.png",width = 480,height = 480)
ggplot( PD,aes(x=int_rate ,y=ITP,fill=loan_outcome))+
        geom_point(aes(shape=loan_outcome,color=loan_outcome,alpha=0.2),size=2)+
        xlab("Interest Rate")+ylab("ITP")
dev.off()

# png(filename = "DIrelationship.png",width = 480,height = 480)
# ggplot( PD,aes(x=ITP ,y=dti,fill=loan_outcome))+
#         geom_point(aes(shape=loan_outcome,color=loan_outcome,alpha=0.2),size=2)+
#         xlab("ITP")+ylab("DTI")+coord_cartesian(xlim = c(0:0.5),ylim = c(0:50))
# dev.off()

#DTI vs loan_statue #problem is there are some dti very high
ggplot( PD,aes(x=loan_status,y=dti,fill=loan_status))+geom_boxplot()+coord_cartesian(ylim = c(0:100))
# ITP
ggplot( PD,aes(x=loan_outcome,y=ITP,fill=loan_outcome))+geom_boxplot()



###
plotdata<-testset
plotdata$term<-as.factor(plotdata$term)
ggplot( plotdata,aes(x=int_rate ,y=payback_rate,fill=term))+
        geom_point(aes(shape=term,color=term,alpha=0.2),size=2)+
        xlab("Interest Rate")+ylab("NRR")
        
