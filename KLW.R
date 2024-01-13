rm(list=ls())
library(parallel)
cl=makeCluster(4)
library(doSNOW)
registerDoSNOW(cl)
setwd("D:/dataset")
train <- read.csv('train.csv',stringsAsFactors = FALSE)
train <- train[complete.cases(train),]
test <- read.csv('test.csv',stringsAsFactors = FALSE)
test$outcome <- 0
total <- rbind(train,test)

#달러변환
total$SBA_Appv<-as.numeric(gsub("[\\$,]","",total$SBA_Appv))
total$GrAppv<-as.numeric(gsub("[\\$,]","",total$GrAppv))
total$DisbursementGross<-as.numeric(gsub("[\\$,]","",total$DisbursementGross))

# 산업코드 처리
total$NAICS<-substr(total$NAICS,1,2)
total$NAICS<-ifelse(total$NAICS=="0",NA,total$NAICS)

# 데이터 가공
total$State<-ifelse(total$State=="",NA,total$State)
total$NewExist<-ifelse(total$NewExist==0,NA,total$NewExist)

# 팩터바꾸기
total$NewExist<-as.factor(as.character(total$NewExist))
total$UrbanRural<-as.factor(as.character(total$UrbanRural))
total$outcome<-as.factor(as.character(total$outcome))
total$LowDoc<-factor(total$LowDoc, levels=c("Y","N"))
total$RevLineCr<-factor(total$RevLineCr,c("Y","N","0","T"))

#날짜 처리
library(lubridate)
Sys.setlocale("LC_TIME","English")
total$ApprovalDate<-as.Date(as.character(total$ApprovalDate),"%d-%b-%y")
total$DisbursementDate<-as.Date(as.character(total$DisbursementDate),"%d-%b-%y")
total$ApprovalFY<-as.character(total$ApprovalFY)
total$ApprovalFY[total$ApprovalFY =="1976A"]=1976
total$ApprovalFY<-as.integer(total$ApprovalFY)
total$ApprovalDateY<-as.integer(year(total$ApprovalDate))

#그룹화
total$NAICS_g<-ifelse(total$NAICS %in% c(23,44,54,72,81),1,ifelse(total$NAICS %in% c(11,21,22,49,55,92),3,2))
total$State_g<-ifelse(total$State == "CA",1,ifelse(total$State %in% c("FL","NY","TX"),2,3))
total$NAICS_g<-as.factor(total$NAICS_g)
total$State_g<-as.factor(total$State_g)


# Total 제거
dlt <- c("NAICS","State","City","Zip","BankState","Bank","ApprovalDate","DisbursementDate","ApprovalFY","GrAppv","SBA_Appv","ApprovalDateM")
total<-total[,!names(total) %in% dlt]

# 다시 split
trainset<-total[1:dim(train)[1],]
testset<-total[(dim(train)[1]+1):(dim(train)[1]+dim(test)[1]),]

#test 채워넣기
library(VIM)
testset1<-testset[,!names(testset) %in% "outcome"]
knnOutput <- VIM::kNN(testset1)
testset1<-knnOutput[,1:dim(testset1)[2]]

trainset<-trainset[complete.cases(trainset),]

#데이터 2개로나누기
library(dplyr)
data1<-trainset %>% filter(ApprovalDateY %in% c(2005,2006,2007))
data2<-trainset %>% filter(!ApprovalDateY %in% c(2005,2006,2007))

data1_test<-testset1 %>% filter(ApprovalDateY %in% c(2005,2006,2007))
data2_test<-testset1 %>% filter(!ApprovalDateY %in% c(2005,2006,2007))

#-------------------------------------------------
library(C50)

fit1<-C5.0(outcome~.-Id,data=data1,trials=15)
fit2<-C5.0(outcome~.-Id,data=data2,trials=15)
data1_test$outcome <- predict(fit1, data1_test, type = "class")
data2_test$outcome <- predict(fit2, data2_test, type = "class")

final_data <- rbind(data1_test,data2_test)
df<-data.frame(Id=final_data$Id, outcome=final_data$outcome)
write.csv(df,"outcome.csv",row.names = FALSE)