library(gmodels)
CrossTable(dataset1$purpose,dataset1$loan_status,prop.r = TRUE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
CrossTable(dataset2$purpose,dataset1$loan_status,prop.r = TRUE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
CrossTable(dataset3$purpose,dataset1$loan_status,prop.r = TRUE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
CrossTable(dataset4$purpose,dataset1$loan_status,prop.r = TRUE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
#data plot
#merge data without dataset4
#
plot.dataset<-select(dataset2016,c(3,4 ,5, 6,7, 10, 11, 12, 13,14,19 ,23 ,24, 25, 26 ,29,30 ,31 ,32, 33,38,15))
#filter loan status to remine Full pay, default, charge off
plot.dataset<-filter(plot.dataset,loan_status =="Fully Paid"|loan_status =="Charged Off"|loan_status =="Default")

########################################################################################################
#Data pre-processing

##time problem rember change in csv about term
plot.dataset$term = factor(plot.dataset$term,
                          levels = c(" 36 months"," 60 months"),
                          labels = c(0.6, 1))
plot.dataset$term<-as.numeric(levels(plot.dataset$term))[plot.dataset$term]

##time problem rember change in csv about credit age
plot.dataset$issue_d<-dmy(paste("01-", plot.dataset$issue_d , sep =""))
plot.dataset$earliest_cr_line<-dmy(paste("01-", plot.dataset$earliest_cr_line , sep =""))
#creat new variable call credit age
plot.dataset<-mutate(plot.dataset,
                    credit_age = difftime(plot.dataset$issue_d,plot.dataset$earliest_cr_line,units="weeks"))
######################################################################################################
#Add new variable
#creat new variable call income to payment ratio ITP
plot.dataset<-mutate(plot.dataset,mic = plot.dataset$annual_inc/12,ITP = (plot.dataset$installment/mic))
z<-plot.dataset$ITP>=1
plot.dataset[z,"ITP"]<- 1
rm(z)
#creat new variable call revol to payment ratio RTI
plot.dataset<-mutate(plot.dataset,RTI =plot.dataset$revol_bal/mic)
B<-plot.dataset$RTI>=4
plot.dataset[B,"RTI"]<- 4
rm(B)
#delet old variable
plot.dataset$mic<-NULL
plot.dataset$revol_bal<-NULL
##################################
#remove some variables
plot.dataset$issue_d<-NULL
plot.dataset$earliest_cr_line<-NULL
#change to %numerice
plot.dataset$int_rate<-as.numeric(sub("%","",plot.dataset$int_rate))/100
plot.dataset$revol_util<-as.numeric(sub("%","",plot.dataset$revol_util))/100

#creat new variable call return_rate and loss_rate 

plot.dataset<-mutate(plot.dataset,expect_return_rate =(1+plot.dataset$int_rate)^(5*plot.dataset$term))
#treasure rate in 2016/01 is about 2.09%, we use it as our risk-free interest rate
plot.dataset<-mutate(plot.dataset,
                    ROI = (1+((plot.dataset$total_pymnt_inv-plot.dataset$funded_amnt_inv)/(plot.dataset$funded_amnt_inv)))^(1/(5*plot.dataset$term)))
summary(plot.dataset$ROI)
#
plot.dataset<-mutate(plot.dataset,
                    NRR = (plot.dataset$total_pymnt_inv)/(plot.dataset$funded_amnt_inv*expect_return_rate))

#replace the gain_rate is big then 1
bg<-plot.dataset$NRR >=1
plot.dataset[bg,"NRR"]<- 1
#change the label
loanStatus<-c("Fully Paid"=1,"Default"=0,"Charged Off"=0)
plot.dataset$loan_status <- loanStatus[plot.dataset$loan_status]
summary(plot.dataset$loan_status)

#check the dataset
#add new label profit
plot.dataset<-mutate(plot.dataset,
                    profit = plot.dataset$funded_amnt_inv-plot.dataset$total_pymnt_inv)
##########################################################

##########################################################
library(lattice)
library(ggplot2)
#credit age VS loss_rate
plot(x =plot.dataset$credit_age,y = plot.dataset$NRR )

##distrubtion density
qplot(x=NRR,                             
      data=plot.dataset,                     
      geom="density",        # 圖形=density
      xlab="NRR",                         
      color= plot.dataset$home_ownership           # 以顏色標home_owner，複合式的機率密度圖
)
#

qplot(x=NRR,                             
      data=plot.dataset,                     
      geom="density",        # 圖形=density
      xlab="NRR",                         
      color= plot.dataset$purpose           # 以顏色標home_owner，複合式的機率密度圖
)


#calculate mean of loss for each grade
plot.dataset%>% group_by(grade) %>%summarise(mean(NRR,na.rm = TRUE))
plot.dataset %>% group_by(grade) %>%summarise(sd(NRR,na.rm = TRUE))

#plot boxplot
#box plot with home ownership
ggplot( plot.dataset,aes(x=loan_status,y=dti,fill=home_ownership))+geom_boxplot()+coord_cartesian(ylim = c(0:100))
ggplot(plot.dataset,aes(x=loan_status,y=ITP,fill=home_ownership))+geom_boxplot()
#grade vs NRR
ggplot( plot.dataset,aes(x=grade,y=NRR,fill=grade))+geom_boxplot()
#term VS NRR
plot.dataset$term<-as.factor(plot.dataset$term)
ggplot( plot.dataset,aes(x=term,y=NRR,fill=term))+geom_boxplot()

#loan status VS NRR
plot.dataset$loan_status<-as.factor(plot.dataset$loan_status)
ggplot( plot.dataset,aes(x=loan_status,y=NRR,fill=term))+geom_boxplot()

####################################################################################################################################
png(filename = "Hrelationship.png",width = 480,height = 480)
#loan_status VS home_ownership                                                                                                   ###
ggplot(plot.dataset , aes(x = home_ownership , y = ..count.. , fill = factor(loan_status , c(1 , 0) , c('Default' , 'Fully Paid')))) +    ###
  geom_bar() +                                                                                                                   ###
  theme(legend.title = element_blank())                                                                                          ###
# for mortga, default=19871, fullpay=72880, 21.4%                                                                                ###
# for own, default=5925, fullpay=16831, 26%
# for RENT, default=22607, fullpay=49025, 31.6%
# any<-filter(PD,home_ownership=="ANY")
# for any, default=8, fullpay=24, 26%
dev.off()
####################################################################################################################################
png(filename = "Purposerelationship.png",width = 590,height = 430)
#loan_status VS purpose
ggplot(plot.dataset , aes(x = purpose , y = ..count.. , fill = factor(loan_outcome , c(1 , 0) , c('Default' , 'Fully Paid')))) + 
        geom_bar() + 
        theme(legend.title = element_blank())
dev.off()
############################
qplot(x=dti,
      
      data=plot.dataset,                     
      geom="density",        # 圖形=density
      xlab="NRR",                         
      color= plot.dataset$home_ownership           # 以顏色標home_owner，複合式的機率密度圖
)

#DTI VS grade, NRR
par(mfrow=c(2,1))
ggplot(plot.dataset,aes(x=dti,y=NRR))+geom_point(aes(color=term),alpha=0.3)+coord_cartesian(xlim = c(0,80))
ggplot(plot.dataset,aes(x=dti,y=NRR))+geom_point(aes(color=term),alpha=0.3)+coord_cartesian(xlim = c(80,500))
#revol
ggplot(plot.dataset,aes(x=return_rate,y=loss_rate))+geom_point(aes(color=grade,alpha=0.3))
##################################################################################################################################
#
#plot
#subsample of plot data
set.seed(7777)
split = sample.split(PD, SplitRatio = 0.001)
newPD = subset(PD, split == TRUE)

#plot Relationship wiht  IR DTI ITP and loan_status
png(filename = "DTI&TR.png",width = 680,height = 480)
ggplot( newPD,aes(x=int_rate ,y=dti,fill=loan_outcome))+
        geom_point(aes(shape=loan_outcome,color=loan_outcome,alpha=0.7),size=4)+
        xlab("Interest Rate")+ylab("DTI")+coord_cartesian(ylim = c(0:100))+theme_classic()
dev.off()
png(filename = "ITP&IR.png",width = 680,height = 480)
ggplot( newPD,aes(x=int_rate ,y=ITP,fill=loan_outcome))+
        geom_point(aes(shape=loan_outcome,color=loan_outcome,alpha=0.4),size=4)+
        xlab("Interest Rate")+ylab("ITP")+coord_cartesian(ylim = c(0:1))+theme_classic()
dev.off()
#plot Relationship wiht  IR NRR period and loan_status
ggplot( newPD,aes(x=int_rate ,y=NRR,fill=period))+
        geom_point(aes(shape=period,color=period,alpha=0.4),size=4)+
        xlab("Interest Rate")+ylab("NRR")+coord_cartesian(ylim = c(0:1))+theme_classic()


# png(filename = "DIrelationship.png",width = 480,height = 480)
# ggplot( PD,aes(x=ITP ,y=dti,fill=loan_outcome))+
#         geom_point(aes(shape=loan_outcome,color=loan_outcome,alpha=0.2),size=2)+
#         xlab("ITP")+ylab("DTI")+coord_cartesian(xlim = c(0:0.5),ylim = c(0:50))
# dev.off()

#DTI vs loan_statue #problem is there are some dti very high
ggplot( PD,aes(x=loan_status,y=dti,fill=loan_status))+geom_boxplot()+coord_cartesian(ylim = c(0:100))
# ITP
ggplot( PD,aes(x=loan_outcome,y=ITP,fill=loan_outcome))+geom_boxplot()

############
#new try
png(filename = "ITP&TR.png",width = 680,height = 480)
p<-ggplot(plot.dataset,aes(int_rate,ITP, shape=factor(loan_status)))
p+geom_point(aes(colour=factor(loan_status)),size=4)+geom_point(colour="grey90",size=1.5,alpha=0.8)+
        xlab("Interest Rate")+ylab("ITP")+coord_cartesian(ylim = c(0:1))+theme_classic()

dev.off()


###
plotdata<-testset
plotdata$term<-as.factor(plotdata$term)
ggplot( plotdata,aes(x=int_rate ,y=payback_rate,fill=term))+
        geom_point(aes(shape=term,color=term,alpha=0.2),size=2)+
        xlab("Interest Rate")+ylab("NRR")
        
