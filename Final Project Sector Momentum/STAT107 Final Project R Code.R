###########################
#  STAT 107 Final Project
###########################
library(quantmod)
library(dplyr)
library(ggplot2)
library(stargazer)
library(PerformanceAnalytics)
library(xlsx)
 
### Assign Fidelity sector funds into distinct industries

# set up working directory
setwd("E:/Course Work at Harvard/Introduction to Financial Statisitcs/Final Project Sector Momentum")

# read in sector funds and industry assignment
funds<-read.csv("Sector Fund Names.csv")
funds[order(funds$Industry.Assigment),]

# industries covered
industry<-unique(funds$Industry.Assigment)

### Obtain the historical prices and monthly return  
# get the monthly return for each fund and store them
return.mat<-matrix(NA,nrow = 120, ncol = nrow(funds))
colnames(return.mat)<-as.character(funds$Ticker)
for(i in 1:nrow(funds)){
  ticker<-as.character(funds$Ticker[i])
  price<-getSymbols(ticker, from="2007-01-01", to="2016-12-03", auto.assign = F)
  return.mat[,i]<-as.numeric(monthlyReturn(price)) 
}
 
### The function Sector.Momentum 
#   inputs: 
#          from which date the trading algorithm begins: from.date
#          to which date the trading algorithm ends: to.date
#          estimation period: est.period
#          holding period: hold.period
#          number of winning sector to hold : n.win.sector


Sector.Momentum<-function(from.date,to.date,est.period,hold.period,n.win.sector){
 

# Initialized a place-holder for annualized returns, this will be the final return object
CAGR<-rep(NA, 120/est.period-1)

### Loop through 1st to last estimation period
for(j in 1:(120/est.period-1)){
 
  # initialize place-holder for annualized return at each estimation period
  funds$comp.ret<-rep(1,nrow(funds)) 
  
  ### At each estimation period j, calculate Compound Return for each fund
  # temprarily store in funds$CAGR, they will change for each iteration of j estimation period
  for(i in 1:nrow(funds)){
    for(k in 1:est.period){
      funds$comp.ret[i]<-funds$comp.ret[i]*(1+return.mat[est.period*(j-1)+k,i])
    }
  }
  ### Now, we have Compound Return for each fund during the estimation period
  ### select holding sectors
    winning.sector<-funds%>%
    group_by(Industry.Assigment)%>%
    summarise(CAGR=mean(comp.ret)^(1/(est.period/12))-1)%>%
    arrange(desc(CAGR))%>%
    top_n(n=n.win.sector)

  # select all funds within the winning sector
  winning.funds<-funds%>%filter(Industry.Assigment %in% winning.sector$Industry.Assigment)%>%select(Ticker)
  n.winning.funds<-length(winning.funds$Ticker)
  
  ### invest for the holding period  
  # select returns from previously created return matrix for those winning funds
  win.return.mat<-as.matrix(return.mat[,as.vector(as.character(winning.funds$Ticker))])
  # Initialize a place-holder for each winning fund's compound return during holding period
  win.funds.comp.ret<-rep(1,n.winning.funds)
  for(m in 1:n.winning.funds){
    for(n in 1:hold.period){
      win.funds.comp.ret[m]<-win.funds.comp.ret[m]*(1+win.return.mat[est.period*j+n,m])
    }
  }
  
  ### Now, we have compound returns for all winning funds during the holding period
  # Assuming equal investing among all funds
  # Calculate the final CAGR of jth estimation period:
  # average of all funds' compound return, then annualize 
  CAGR[j]<-mean(win.funds.comp.ret)^(1/(hold.period/12))-1
}

return(CAGR)
}
 
 
### Use the function Sector.Momentum, vary estimation and hold periods, method, and number of winning sectors

est.period<-c()
hold.period<-c()
n.win.sector<-c()
CAGR<-c()
# Let estimation and hold periods to be equal, and loop through 1 to 12 month
for(i in 1:12){
  
  # The total number of sectors is 23, we want to hold at least somewhat better than average
  # Let the number of winning sectors loop through 1:10
  for(j in 1:23){
    CAGR.temp<-Sector.Momentum(from.date="2007-01-01", to.date="2016-12-03", est.period=i, hold.period=i,n.win.sector=j)
    est.period<-c(est.period,rep(i,length(CAGR.temp)))
    hold.period<-c(hold.period,rep(i,length(CAGR.temp)))
    n.win.sector<-c(n.win.sector,rep(j,length(CAGR.temp)))
    CAGR<-c(CAGR,CAGR.temp)
  }
}

# Put them into a data.frame
momentum.CAGR<-data.frame(CAGR,est.period,hold.period,n.win.sector,method=rep("Sector Momentum",length(CAGR)))

### If buy and hold S&P500 for the same length, such as 1-month, 2-month, and so on, what is the CAGRs?
price<-getSymbols("SPY", from="2007-01-01", to="2016-12-03", auto.assign = F)
ret<-as.numeric(monthlyReturn(price)) 
period<-c()
CAGR<-c()
for(i in 1:12){
  for(j in 1:(120/i)){
    CAGR.temp<-1
    for(m in 1:i){CAGR.temp<-CAGR.temp*(1+ret[i*(j-1)+m])}
    CAGR.temp<-CAGR.temp^(1/(i/12))-1
    CAGR<-c(CAGR, CAGR.temp)
    period<-c(period,i)
  }
}

# Put them into a data.frame
SP500.CAGR<-data.frame(CAGR, est.period=period,hold.period=period,n.win.sector=rep(NA,length(CAGR)),method=rep("S&P500",length(CAGR)))
 
### Make a Table of comparison of various strategies
all.CAGR<-rbind(momentum.CAGR,SP500.CAGR )
temp<-all.CAGR%>%group_by(method, est.period, n.win.sector)%>%
  summarise(Ave.CAGR=mean(CAGR), std.dev=sd(CAGR), Sharpe.ratio=mean(CAGR)/sd(CAGR))%>%
  as.data.frame()
# output to xls file
write.csv(temp,"Table 3.csv")

 
### Visualize

# X-axis is  Estimation & Hold Period(Month), Y is CAGR
temp%>%
  ggplot(aes(x=est.period, y=Ave.CAGR, colour=factor(n.win.sector)))+
  geom_point(aes(size=Ave.CAGR))+
  geom_line()+ 
  theme(panel.background=element_blank(),
        plot.title = element_text(size=rel(1.5),colour = "red"),
        legend.position = "bottom")+
  ggtitle("Figure 1: Compound Annualized Growth Rates vs Estimation & Hold Period(Month)")+
  scale_x_continuous(breaks=seq(1,12,2))+
  xlab("Estimation & Hold Period (Month)")+
  ylab("Average CAGR")+
  facet_wrap(~method) 

# X-axis is  Estimation & Hold Period(Month), Y is Sharpe Ratio
temp%>%
  ggplot(aes(x=est.period, y=Sharpe.ratio, colour=factor(n.win.sector)))+
  geom_point(aes(size=Ave.CAGR))+
  geom_line()+ 
  theme(panel.background=element_blank(),
        plot.title = element_text(size=rel(1.5),colour = "red"),
        legend.position = "bottom")+
  ggtitle("Figure 2: Sharpe.ratio vs Estimation & Hold Period(Month)")+
  scale_x_continuous(breaks=seq(1,12,2))+
  xlab("Estimation & Hold Period (Month)")+
  ylab("Sharpe.ratio")+
  facet_wrap(~method)


# X-axis is Number of Winning Sectors to Hold
momentum.CAGR%>%
  group_by(est.period, n.win.sector)%>%
  summarise(Ave.CAGR=mean(CAGR))%>%
  ggplot(aes(x=n.win.sector, y=Ave.CAGR, colour=factor(est.period)))+
  geom_point(aes(size=Ave.CAGR))+
  geom_line()+
  theme(panel.background=element_blank(),
        plot.title = element_text(size=rel(1.5),colour = "red"),
        legend.position = "bottom")+
  ggtitle("Figure 3: Compound Annualized Growth Rates vs Number of Winning Sectors to Hold")+
  xlab("Number of Winning Sectors to Hold")+
  ylab("Average CAGR") 

### Regression

# From the above visualization, we could hypothesis that, the length of estimation and hold periods has a negative effect on returns, even adjusting for number of winning sectors to hold; and only holding the top 1 sector is better than any other strategies, and the effect of number of sectors to hold may be slightly negative or even non-significant, after accounting for estimation and hold period.
fit1<-lm(CAGR~factor(est.period)+factor(n.win.sector),data=momentum.CAGR)
fit2<-lm(CAGR~est.period+n.win.sector,data=momentum.CAGR)
fit3<-lm(CAGR~factor(method)+est.period,data=all.CAGR)
 
stargazer(fit1,out="fit1.htm",title="Model 1: Linear Regression (Compare among Momentum Strategies)",
          covariate.labels = c("2-month","3-month","4-month","5-month","6-month","7-month",
                               "8-month","9-month","10-month","11-month","12-month",
                               "2 Sectors","3 Sectors","4 Sectors","5 Sectors","6 Sectors",
                               "7 Sectors","8 Sectors","9 Sectors","10 Sectors","11 Sectors",
                               "12 Sectors","13 Sectors","14 Sectors","15 Sectors","16 Sectors",
                               "17 Sectors","18 Sectors","19 Sectors","20 Sectors","21 Sectors",
                               "22 Sectors","23 Sectors"))

stargazer(fit2,out="fit2.htm",title="Model 2: Linear Regression (Compare among Momentum Strategies)",
          covariate.labels = c("Period","Number of Winning Sectors to Hold"))

stargazer(fit3,out="fit3.htm",title="Model 3: Linear Regression (Compare Momentum Strategies with S%P500)",
          covariate.labels = c("Trading Strategy=S&P 500","Period"))

 