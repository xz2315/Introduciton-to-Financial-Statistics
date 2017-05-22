####################
# HW 6
# Xiner Zhou
# 11/3/2016
####################

# JKD : Large Core Index Fund
# JKG : Mid Cap Core Index Fund
# JKJ : Small Cap Core Index Fund
# EFA : International Index Fund
# AGG : Aggregate Bond Fund
# SHY: Short Term Bond Fund
library(quantmod)

getSymbols("JKD", from="2010-09-01")
getSymbols("JKG", from="2010-09-01")
getSymbols("JKJ", from="2010-09-01")
getSymbols("EFA", from="2010-09-01")
getSymbols("AGG", from="2010-09-01")
getSymbols("SHY", from="2010-09-01")
jkd=monthlyReturn(Ad(JKD))[1:36,]
jkg=monthlyReturn(Ad(JKG))[1:36,]
jkj=monthlyReturn(Ad(JKJ))[1:36,]
efa=monthlyReturn(Ad(EFA))[1:36,]
agg=monthlyReturn(Ad(AGG))[1:36,]
shy=monthlyReturn(Ad(SHY))[1:36,]

# Dodge and Cox Balanced Fund (DODBX)
# Fidelity New Millenium Fund (FMILX) 
# Fidelity Short Term Bond Fund (FSHBX)
getSymbols("DODBX", from="2010-09-01")
getSymbols("FMILX", from="2010-09-01")
getSymbols("FSHBX", from="2010-09-01")
dodbx=monthlyReturn(Ad(DODBX))[1:36,]
fmilx=monthlyReturn(Ad(FMILX))[1:36,]
fshbx=monthlyReturn(Ad(FSHBX))[1:36,]

# write it all to a csv file  
setwd("E:/Course Work at Harvard/Introduction to Financial Statisitcs")
write.csv(cbind(jkd, jkg, jkj, efa, agg, shy, dodbx, fmilx, fshbx ),'RBSA.csv')
