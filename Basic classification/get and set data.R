#setwd to github direction
setwd("C:/Users/glab/Documents/GitHub/P2P_default_risk")
#check dataset
if (!file.exists("dataset")){
        dir.create("dataset")
}
#set the download from web
file2016Q1<-"https://resources.lendingclub.com/LoanStats_2016Q1.csv.zip"
download.file(file2016Q1,destfile ="./dataset/2016Q1.csv",method = "curl" )
file2016Q2<-"https://resources.lendingclub.com/LoanStats_2016Q2.csv.zip"
download.file(file2016Q2,destfile ="./dataset/2016Q2.csv",method = "curl" )
file2016Q3<-"https://resources.lendingclub.com/LoanStats_2016Q3.csv.zip"
download.file(file2016Q3,destfile ="./dataset/2016Q3.csv",method = "curl" )
file2016Q4<-"https://resources.lendingclub.com/LoanStats_2016Q4.csv.zip"
download.file(file2016Q4,destfile ="./dataset/2016Q4.csv",method = "curl" )
file2017Q1<-"https://resources.lendingclub.com/LoanStats_2017Q1.csv.zip"
download.file(file2017Q1,destfile ="./dataset/2017Q1.csv",method = "curl" )
file2017Q2<-"https://resources.lendingclub.com/LoanStats_2017Q2.csv.zip"
download.file(file2017Q2,destfile ="./dataset/2017Q2.csv",method = "curl" )
file2017Q3<-"https://resources.lendingclub.com/LoanStats_2017Q3.csv.zip"
download.file(file2017Q3,destfile ="./dataset/2017Q3.csv",method = "curl" )
file2017Q4<-"https://resources.lendingclub.com/LoanStats_2017Q4.csv.zip"
download.file(file2017Q4,destfile ="./dataset/2017Q4.csv",method = "curl" )
file2018Q1<-"https://resources.lendingclub.com/LoanStats_2018Q1.csv.zip"
download.file(file2018Q1,destfile ="./dataset/2018Q1.csv",method = "curl" )
#list all files in dataset
list.files("./dataset")
#check the download date
dateDownloaded<-data()
#unZip data
unzip("2016Q1.csv")
unzip("2016Q2.csv")
unzip("2016Q3.csv")
unzip("2016Q4.csv")
unzip("2017Q1.csv")
unzip("2017Q2.csv")
unzip("2017Q3.csv")
unzip("2017Q4.csv")
unzip("2018Q1.csv")
#read data and skip first 2 row becaue they are describe line and because header is true so skip =1)
Q12016<-read.csv(("LoanStats_2016Q1.csv"),header = TRUE,sep = ",",skip = 1)
Q22016<-read.csv(("LoanStats_2016Q2.csv"),header = TRUE,sep = ",",skip = 1)
Q32016<-read.csv(("LoanStats_2016Q3.csv"),header = TRUE,sep = ",",skip = 1)
Q42016<-read.csv(("LoanStats_2016Q4.csv"),header = TRUE,sep = ",",skip = 1)
Q12017<-read.csv(("LoanStats_2017Q1.csv"),header = TRUE,sep = ",",skip = 1)
Q22017<-read.csv(("LoanStats_2017Q2.csv"),header = TRUE,sep = ",",skip = 1)
Q32017<-read.csv(("LoanStats_2017Q3.csv"),header = TRUE,sep = ",",skip = 1)
Q42017<-read.csv(("LoanStats_2017Q4.csv"),header = TRUE,sep = ",",skip = 1)
Q12018<-read.csv(("LoanStats_2016Q1.csv"),header = TRUE,sep = ",",skip = 1)

#check data size
object.size()
