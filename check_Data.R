tbl_df("Dataset")
#check default and full paydata
default_data<-filter(Dataset,statue=="default"|"charge off")
full_pay_data<-filter(Dataset,statue =="full pay")
#check the mean dti of all default and full pay
summarize(default,mean(dti))
summarise(full_pay_data,menu(dti))