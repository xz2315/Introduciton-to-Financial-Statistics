library(quantmod)
library(BSDA)
library(PerformanceAnalytics)

getSymbols("SNA",from="2015-01-01",to="2015-12-31")
rets<-dailyReturn(Ad(SNA))
dates<-time(rets)
wdays<-weekdays(as.Date(dates,'%d-%m-%Y'))
mon=1.0*(wdays=="Monday")
tues=1.0*(wdays=="Tuesday")
wed=1.0*(wdays=="Wednesday")
thur=1.0*(wdays=="Thursday")
fri=1.0*(wdays=="Friday")
fit<-lm(rets~tues+wed+mon+thur)
fit

600/(600+120)

1.2-0.4*1.6+0.4*1.25

0.6*(0.7*0.2+0.3*0.04)+0.4*(0.7*0.06+0.3*0.24)

2500*(1+0.04)^5

1-pnorm(0.3,mean=0.16,sd=0.1)

a<-c(11,16,5,2,9)
sd(a)
sqrt(sum((a-mean(a))^2)/4)

0.2*0.55+0.1*0.2-0.2*0.25

a<-c(0.6,0.2,-0.1,-0.3)
b<-c(0.15,0.5,0.25,0.1) 
c<-(a-mean(a))^2
b%*%c

0.25*0.8+0.25*1.2+0.25*0+0.25*1

(2000/1000)^(1/5)-1
1*(1.1487)^5

ret<-c(22,10,20,5)
risk<-c(25,8,18,12)
sharpe<-(ret-6)/risk
beta<-c(0.5,1.6,1,1)
trenor<-(ret-6)/beta
 
means=c(.05,.18,.35)
sds <-c(.07,.13,.16)  
wt<-matrix(c(0.1,0.8,0.1),nrow=3,ncol=1,byrow=T)
corrs=matrix(nrow=3,ncol=3,c(1,.3956,.0893,.3596,1,.1731,.0893,.1731,1),byrow=TRUE)
 
b<-sds%*%t(sds)
cov<-b*corrs
risk<-sqrt(t(wt)%*%cov%*%wt)

efficient.portfolio(means, cov, 0.184)
globalMin.portfolio(means, cov)
tangency.portfolio(means, cov, 0.025)


7/13

res<-numeric(10000)
for(i in 1:100000){
  temp<-sample(seq(1,6,1), size=3, replace = TRUE, prob = rep(1/6,6))
  if(temp[3]>min(temp[1],temp[2])&temp[3]<max(temp[1],temp[2])){res[i]<-1}else{temp[i]<-0}
}
mean(res)

 
fname="http://people.fas.harvard.edu/~mparzen/stat107/logisticquest.csv"
mydata=read.csv(fname)
postomorrow=mydata[,2]
gold=mydata[,3]
prevret=mydata[,4]

fit<-glm(postomorrow~gold+prevret,family=binomial(logit))
summary(fit)
coef<-coefficients(fit)
val<-c(1,1000,-0.2)
exp(coef%*%val)/(1+exp(coef%*%val))
 


getSymbols("SNA",from="2013-01-01")
getSymbols("SPY",from="2013-01-01")
snaret=monthlyReturn(Ad(SNA))
spyret=monthlyReturn(Ad(SPY))
fit<-lm(snaret~spyret)
confint(fit)

getSymbols("IBM",from="2015-01-01")
getSymbols("SNA",from="2015-01-01")
var.test(dailyReturn(Ad(IBM)),dailyReturn(Ad(SNA)))

getSymbols("JNJ",from="2015-01-01",to="2015-12-31")
ret<-dailyReturn(Ad(JNJ))
ret5<-lag(ret,k=5)
rets<-data.frame(ret,ret5)
View(rets)
ret<-ret[-c(1:5)]
ret5<-ret5[-c(1:5)]
rets<-data.frame(ret,ret5)
View(rets)
cor(as.numeric(ret),as.numeric(ret5))
library(timeSeries)
acf(ret)
lm(ret~ret5)
 
library(quantmod)
library(tseries)
library(PerformanceAnalytics)
# Calculates the last day cointegration p-value (ADF test) and Correlation, using prior 180 days
myvals=function(s1,s2) {
  ap1=Ad(getSymbols(s1,auto.assign=FALSE,from="2013-01-02"))
  ap2=Ad(getSymbols(s2,auto.assign=FALSE,from="2013-01-02"))
  n=length(ap2)
  nn=n-179
  vals=1:nn
  i=n
  p1 = ap1[(i-179):i]
  p2 = ap2[(i-179):i]
  fit=lm(p1~-1+p2)
  beta=coef(fit)[1]
  sprd=p1-beta*p2
  sprd=as.numeric(sprd)
  cat("Cointegration p-value = ",adf.test(sprd,alternative="stationary",k=0)$p.value,"\n")
  cat("Correlation = ",cor(p1,p2),"\n")
}
myvals("IBM","JNJ")

tan.ret<-0.6*0.14+0.4*0.1

(0.124-0.11)/(0.124-0.05)


0.036-0.063

(0.14-0.06)/0.22


integrand <- function(x) {log(x)/sqrt(x)}
integrate(integrand, lower = 0, upper = 1)

x <- 1:20
y <- x + rnorm(20)
X<-cbind(rep(1,length(x)), x)
solve(t(X)%*%X)%*%t(X)%*%y
fit<-lm(y~x)
summary(fit)

X<-cbind(rep(1,length(x)), x, x^2)
solve(t(X)%*%X)%*%t(X)%*%y
fit<-lm(y~x+I(x^2))
summary(fit)


X<-cbind(rep(1,length(x)), x, x^2, x^3)
solve(t(X)%*%X)%*%t(X)%*%y
fit<-lm(y~x+I(x^2)+I(x^3))
summary(fit)


X<-cbind(rep(1,length(x)), x, x^2, x^3, x^4)
solve(t(X)%*%X)%*%t(X)%*%y
fit<-lm(y~x+I(x^2)+I(x^3)+I(x^4))
summary(fit)

X<-cbind(rep(1,length(x)), x, x^2, x^3, x^4, x^5, x^6)
solve(t(X)%*%X)%*%t(X)%*%y
fit<-lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6))
summary(fit)



getSymbols("AAPL",from="2013-01-01",to="2015-12-31")
aaplret=monthlyReturn(Ad(AAPL))
theta <- function(d,i){return(SharpeRatio(d[i],FUN="StdDev"))} 
SharpeRatio(aaplret,FUN="StdDev")
library(boot)
fit<-boot(aaplret,theta,10000) 

boot.ci(fit)

library(logspline)
getSymbols("AAPL",from="2015-01-01",to="2015-12-31")
aaplret=dailyReturn(Ad(AAPL))
hist(aaplret)
x=seq(-.02,.02,.01)
fit<-logspline(as.numeric(aaplret))
norm<-dnorm(x, mean = mean(aaplret), sd = sd(aaplret))
sp<-dlogspline(x,fit)
sum(sp-norm)

aaplret=dailyReturn(Ad(AAPL))
aaplret<-(aaplret-mean(aaplret))/sd(aaplret)
fit<-logspline(as.numeric(aaplret))
norm<-dnorm(x, mean = mean(aaplret), sd = sd(aaplret))
sp<-dlogspline(x,fit)
sum(sp-norm)
