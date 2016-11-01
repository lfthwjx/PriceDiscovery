library(xlsx)
setwd("D:\\UM\\R\\calGARCH\\data\\futures liquidity\\futures volume")
au<-read.xlsx2(".\\Au\\FUT_Fdt.xls", sheetIndex=1,startRow = 1, endRow = 21889, header=FALSE)

au.date<-au[,1]
au.volume<-au[,6]
au.volume<-levels(au.volume)[au.volume]
au.volume<-as.integer(au.volume)
au.agg <- aggregate(au.volume~au.date,au,max)
au[,6]<-au.volume
colnames(au.agg)<-c("X1","X6")
au.max <- merge(au.agg,au)
View(au.max)
au.max<-au.max[,c(1,3,4,5,6,2,7,8)]
write.xlsx(au.max, ".\\Au\\qAu.xls",sheetName = "maximum volume", col.names = FALSE, row.names = FALSE)
