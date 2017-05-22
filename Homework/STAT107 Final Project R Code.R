###########################
#  STAT 107 Final Project
###########################
library(quantmod)
library(dplyr)
install.packages("dplyr")

# Step 1: Assign Fidelity sector funds into distinct industries
funds<-read.csv("/Users/xiz933/Desktop/Sector Fund Names.csv")
industry<-unique(funds$Industry.Assigment)

# Step 2: Obtain the historical prices and monthly return from 2006-01-01 to 2016-12-03
from.date<-"2007-01-01"
to.date<-"2016-12-03"
funds$CAGR<-rep(NA,35)
return.matrix<-matrix(NA,nrow = 120, ncol = 35)
colnames(return.matrix)<-as.character(funds$Ticker)
est.period<-3
hold.period<-3
n.win.sector<-6
CAGR<-rep(NA, 120/est.period)

for(j in 1:(120/est.period-1)){
    j<-2
for(i in 1:20){
   
   
   
  ticker<-as.character(funds$Ticker[i])
  price<-getSymbols(ticker, from=from.date, to=to.date,auto.assign = F)
 
  return.matrix[,i]<-as.numeric(monthlyReturn(price)) 
   
  funds$CAGR[i]<-((1+return.matrix[3*(j-1)+1,i])*(1+return.matrix[3*(j-1)+2,i])*(1+return.matrix[3*(j-1)+3,i]))^(1/(est.period/12))-1
}

# return for each sector
winning.sector<-funds%>%
    group_by(Industry.Assigment)%>%
    summarise(CAGR=mean(CAGR))%>%
    arrange(desc(CAGR))%>%
    top_n(n=n.win.sector)
  
# select all funds within the winning sector
winning.funds<-funds%>%filter(Industry.Assigment %in% winning.sector$Industry.Assigment)%>%select(Ticker)
n.winning.funds<-length(winning.funds$Ticker)

# invest for the holding period  
win.return.matrix<-return.matrix[,as.vector(as.character(winning.funds$Ticker))]
each.CAGR<-rep(NA,n.winning.funds)
for(m in 1:n.winning.funds){
  each.CAGR[m]<-((1+win.return.matrix[3*(j-1)+4,m])*(1+win.return.matrix[3*(j-1)+5,m])*(1+win.return.matrix[3*(j-1)+6,m]))^(1/(hold.period/12))-1
}
# average all funds to get overall CAGR
CAGR[j]<-mean(each.CAGR)
 
}

