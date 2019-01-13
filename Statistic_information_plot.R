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
########
##time problem rember change in csv about credit age1``
PD$issue_d<-dmy(paste("01-", PD$issue_d , sep =""))
PD$earliest_cr_line<-dmy(paste("01-", PD$earliest_cr_line , sep =""))
#creat new variable call credit age
PD<-mutate(PD,
                credit_age = difftime(PD$issue_d,PD$earliest_cr_line,units="weeks"))
##time problem rember change in csv about term
PD$term = factor(PD$term,
                      levels = c(" 36 months"," 60 months"),
                      labels = c(0.6, 1))
#
PD$term<-as.numeric(levels(PD$term))[PD$term]
#creat new variable call return_rate and new label
#change to %numerice
PD$int_rate<-as.numeric(sub("%","",PD$int_rate))/100
PD<-mutate(PD,return_rate =(1+PD$int_rate)^(2*PD$term))
PD<-mutate(PD,
              loss_rate = log(PD$total_pymnt_inv/(PD$funded_amnt_inv*PD$return_rate)))
#replace the loss_rate is small then -1
inf<-PD$loss_rate<=-1
PD[inf,25]<- -1
##########################################################
PD = PD %>%
  mutate(loan_outcome = ifelse(loan_status %in% c('Charged Off' , 'Default') , 
                               1, 
                               ifelse(loan_status == 'Fully Paid' , 0 , 'No info')
  ))
##########################################################
#plot
par(mfrow=c(1,1))
#plot different grade
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

ggplot( PD,aes(x=grade,y=loss_rate,fill=grade))+geom_boxplot()
#term VS loss_rate
PD$term<-as.factor(PD$term)
ggplot( PD,aes(x=term,y=loss_rate,fill=term))+geom_boxplot()
#loan_status VS grade
ggplot(PD , aes(x = grade , y = ..count.. , fill = factor(loan_outcome , c(1 , 0) , c('Default' , 'Fully Paid')))) + 
  geom_bar() + 
  theme(legend.title = element_blank())
#DTI VS grade, loss_rate
par(mfrow=c(2,1))
ggplot(PD,aes(x=dti,y=loss_rate))+geom_point(aes(color=term),alpha=0.3)+coord_cartesian(xlim = c(0,80))
ggplot(PD,aes(x=dti,y=loss_rate))+geom_point(aes(color=term),alpha=0.3)+coord_cartesian(xlim = c(80,500))
#revol
ggplot(PD,aes(x=return_rate,y=loss_rate))+geom_point(aes(color=grade,alpha=0.3))


###
plotdata<-testset
plotdata$term<-as.factor(plotdata$term)
ggplot( plotdata,aes(x=int_rate ,y=payback_rate,fill=term))+
        geom_point(aes(shape=term,color=term,alpha=0.2),size=2)+
        xlab("Interest Rate")+ylab("NRR")
        
