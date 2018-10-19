#plot DTI IR
default<-filter(dataset2016,loan_status =="Default")
Charged_Off<-filter(dataset2016,loan_status  == "Charged Off")
full<-filter(dataset2016,loan_status =="Fully Paid")
tdefau<-filter(dataset2016,loan_status  == "Charged Off"|loan_status  == "Default")
tt<-mean(tdefau$dti,na.rm = TRUE)
options(repr.plot.width=6, repr.plot.height=4)
dataset2016$dti<-dataset2016$dti/100
Vdata<-filter(dataset2016,verification_status !="Not Verified")
m_full<-mean(full$dti,na.rm = TRUE)
default<-mean(default$dti,na.rm = TRUE)
Charged_Off<-mean(Charged_Off$dti,na.rm = TRUE)
ggplot(Vdata, aes(x = dti, y = loan_status, color = loan_status)) + 
        geom_point(alpha=0.5)+theme_light()+ theme(axis.text.x = element_text(colour="grey20",size=12,),
                                                   axis.text.y = element_text(colour="grey20",size=18),  
                                                   axis.title.x = element_text(colour="grey20",size=18),
                                                   axis.title.y = element_text(colour="grey20",size=18))
ggplot(dataset2016, aes(x = ITP, y = loan_status, color = loan_status)) + 
        geom_point(alpha=0.5)+theme_light()+theme(axis.text.x = element_text(colour="grey20",size=12,),
                                                  axis.text.y = element_text(colour="grey20",size=18),  
                                                  axis.title.x = element_text(colour="grey20",size=18),
                                                  axis.title.y = element_text(colour="grey20",size=18))
