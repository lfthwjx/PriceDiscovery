library(FinTS)
library(xlsx)
library(quantmod)
library(fGarch)
#library(WriteXLS)
library(timeSeries)
setwd("D:\\UM\\calGARCH\\macro uncertainty 2nd")
#install.packages("fGarch")
#install.packages("FinTS")

#gdp_season<-read.xlsx("D:\\UM\\calGARCH\\macro uncertainty 2nd\\GDP seasonly 92.03-16.06.xls",sheetIndex=1,startRow =  3, endRow = 100)
#View(gdp_season)

cpi_month<-read.xlsx("D:\\UM\\calGARCH\\macro uncertainty 2nd\\CPI monthly 90.01-16.06.xls", sheetIndex=1,startRow = 2, endRow = 320)

#View(cpi_month)
cpi <- xts(cpi_month[,2], order.by=cpi_month[,1])

#cpi<-as.xts(cpi_month)
#log(cpi)
ptm.cpi<-diff(cpi)

#View(ptm.cpi)
ptm.cpi<-ptm.cpi[-1,]

#index(ptm.cpi)
#substr(index(ptm.cpi),4,8)
Box.test(ptm.cpi,lag=12,type = 'Ljung-Box')
#Box.test(ptm.cpi^2,lag=12,type = 'Ljung-Box')
#?Box.test
ArchTest(x=ptm.cpi,lags=12)

epst.cpi<-ptm.cpi-mean(ptm.cpi)


acf(as.numeric(epst.cpi)^2,lag.max = 20,main="squared series")
pacf(as.numeric(epst.cpi)^2,lag.max = 20,main="squared series")


GARCH.model_cpi<-garchFit(~garch(1,1),data=ptm.cpi,trace=FALSE)
summary(GARCH.model_cpi)

#plot(GARCH.model_1)

vol_1<-fBasics::volatility(GARCH.model_cpi)
#View(vol_1)
#sres_1<-residuals(GARCH.model_1,standardize=TRUE)

vol_1.ts<-ts(vol_1,frequency = 12,start = c(1990,1))
vol.ts<-as.timeSeries(vol_1.ts)
#sres_1.ts<-ts(sres_1,frequency = 12, start = c(1990,2))
#sres.ts<-as.timeSeries(sres_1.ts)

vol_out<-as.data.frame(vol.ts)
#sres_out<-as.timeSeries(sres.ts)
#View(vol_out)
#str(vol_out)
#WriteXLS(vol_out, "D:\\UM\\calGARCH\\output\\cpi.xlsx",SheetNames="vol")
write.xlsx(vol_out, "D:\\UM\\calGARCH\\output\\cpi.xlsx",sheetName = "CPI voltality monthly")
#View(vol_out[,1])
vol_out_tmp<-t(as.matrix(vol_out[c(1:315),1]))

#View(vol_out_tmp)
vol_season<-matrix(vol_out_tmp,ncol  = 3,byrow = TRUE)
#View(vol_season)
vol_season_mean<-rowMeans(vol_season)
#View(vol_season_mean)
#str(vol_season_mean)
#seriesData(vol_1.ts)
#is.timeSeries(vol.ts)
#cpi_date<-seq(as.Date("1990-03-31"), as.Date("2016-03-31"), by = "3 months")
#cpi_date<-t(cpi_date)
#View(cpi_date)
#cpi_vol<-c(cpi_date,vol_season_mean)
vol_season.ts<-ts(vol_season_mean,frequency = 4,end = c(2016,2))
#str(vol_season.ts)
#View(vol_season.ts)
vol_season.ts<-as.timeSeries(vol_season.ts)
#View(vol_season.ts)
write.xlsx(vol_season.ts, "D:\\UM\\calGARCH\\output\\cpi.xlsx",sheetName = "CPI voltality seasonly",append=TRUE)
plot(vol_season.ts)