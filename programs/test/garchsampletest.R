library(quantmod)
getSymbols('^HSI',from = '1989-12-01',to='2013-11-30')
str(HSI)
dim(HSI)
names(HSI)
HSI<-as.xts(HSI)

ptd.HSI<-HSI$HSI.Adjusted
rtd.HSI<-diff(log(ptd.HSI))*100
str(ptd.HSI)
