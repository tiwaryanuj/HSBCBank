library(dplyr)
library(stringr)

DialerData <- read.csv(file.choose(), header=T, sep=",")
TransData <- read.csv(file.choose(), header=T, sep=",")

summary(DialerData$DispositionCode)
#BadNumber               PTP               RPC           SitTone        ThirdPartyContact 
#     2000              2412               804              3200              1584

length(unique(DialerData$AccountNo))
# [1] 250 
#There are call details of 250 unique customers

length(unique(TransData$AccountNo))
#[1] 250
#There are payment details of 250 unique customers

#change date to numeric to find count of calls per account
DialerData$CallDateNum = str_replace_all(DialerData$CallDate, "/", "")
DialerData$CallDateNum <- as.integer(DialerData$CallDateNum)

#Aggregate DialerData to account level
Dailer_grouped = group_by(DialerData, AccountNo )
DialCompl <- summarise(Dailer_grouped,sum(DialCompleted))
DialCompl <- as.data.frame(DialCompl)
DialConnect <- summarise(Dailer_grouped,sum(DialConnect))
DialConnect <- as.data.frame(DialConnect)
DialRPC <- summarise(Dailer_grouped,sum(RPC))
DialRPC <- as.data.frame(DialRPC)
DialPTP <- summarise(Dailer_grouped,max(PTP))
DialPTP <- as.data.frame(DialPTP)
DailCOunt<- tally(group_by(DialerData, AccountNo))
DailCOunt <- as.data.frame(DailCOunt)

DailerAgg <- inner_join(DialCompl,DialConnect,by="AccountNo")
DailerAgg <- inner_join(DailerAgg,DialRPC,by="AccountNo")
DailerAgg <- inner_join(DailerAgg,DialPTP,by="AccountNo")
DailerAgg <- inner_join(DailerAgg,DailCOunt,by="AccountNo")

#Aggregate TransData to account level
TransData$PaymentAmount <- as.integer(TransData$PaymentAmount)
TransData_grouped = group_by(TransData, AccountNo )
AvgTotalAmount<- summarise(TransData_grouped,mean(PaymentAmount))
AvgTotalAmount<- as.data.frame(AvgTotalAmount)

#Join Dialer and Transcastion data on Account# field
DialTransJoin <- inner_join(DailerAgg,AvgTotalAmount,by="AccountNo")

write.csv(DialTransJoin, "FinalDataset_Modeling.csv", row.names = F)

#Exploratory Data Analysis
Final <- read.csv(file.choose(), header=T, sep=",")
summary(Final$sum.DialCompleted.)
summary(Final$sum.DialConnect.)
summary(Final$sum.RPC.)
summary(Final$n)
summary(Final$mean.PaymentAmount.)
summary(Final$max.PTP.)

x1<-Final$mean.PaymentAmount.
h1<-hist(Final$mean.PaymentAmount.)
xfit<-seq(min(x1),max(x1),length=40) 
yfit<-dnorm(xfit,mean=mean(x1),sd=sd(x1)) 
yfit <- yfit*diff(h1$mids[1:2])*length(x1) 
lines(xfit, yfit, col="blue", lwd=2)

