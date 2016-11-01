library(xlsx)
library(xts)
library(timeSeries)
setwd("C:\\UM\\R\\calGARCH\\data\\futures liquidity\\futures volume v2")
dire.names<-list.dirs(getwd(),full.names = FALSE)[-1]
for (j in (1:length(dire.names))) {
  #j=1
  au.file<-dire.names[j]
  au.path<-paste0("./",au.file,seq='')
  #au.path
  au<-read.xlsx2(paste0(au.path,'/FUT_Fdt.xls'), sheetIndex=1,startRow = 1, colIndex = c(1:16), header=FALSE)
  endrow<-dim(au)[1]-3
  au<-read.xlsx2(paste0(au.path,'/FUT_Fdt.xls'), sheetIndex=1,startRow = 1,endRow = endrow, colIndex = c(1:16), header=FALSE)
  #View(au)
  #select for the major contact, which has the most volume
  au.date<-au[,1]
  au.volume<-au[,13]
  au.volume<-levels(au.volume)[au.volume]
  au.volume<-as.integer(au.volume)
  au.agg <- aggregate(au.volume~au.date,au,max)
  au[,13]<-au.volume
  colnames(au.agg)<-c("X1","X13")
  au.max <- merge(au.agg,au)
  #View(au.max)
  au.max<-au.max[,c(1,3,4,5,6,7,8,9,10,11,12,13,2,14,15,16)]
  #au.org<-au.max
  write.xlsx2(au.max, paste0('C:\\UM\\R\\calGARCH\\output\\futures voo\\',au.file,'/q',au.file,'.xls'),sheetName = "maximum volume", col.names = FALSE, row.names = FALSE)
  
  #calculate for the logarithm return of the closed price for the major contact seasonally
  #au.max<-au.org
  au.max[,6]<-as.numeric(levels(au.max[,6])[au.max[,6]])
  au.max[,7]<-as.numeric(levels(au.max[,7])[au.max[,7]])
  au.max[,8]<-as.numeric(levels(au.max[,8])[au.max[,8]])
  au.max[,9]<-as.numeric(levels(au.max[,9])[au.max[,9]])
  au.max[,14]<-as.numeric(levels(au.max[,14])[au.max[,14]])
  au.max[,16]<-as.numeric(levels(au.max[,16])[au.max[,16]])
  au.max[,1]<-as.Date(levels(au.max[,1])[au.max[,1]])
  au.price<-xts(cbind(au.max[,6],au.max[,7],au.max[,8],au.max[,9],au.max[,13],au.max[,14],au.max[,16]),order.by=au.max[,1])
  au.il1.daily<-abs((au.max[,6]-au.max[,9])/(au.max[,13]/10000))
  au.il2.daily<-(au.max[,7]-au.max[,8])/au.max[,6]/(au.max[,13]/10000)
  au.close.late<-au.max[,9]
  au.close.late[2:(length(au.close.late)+1)]<-au.close.late
  au.close.late[1]<-1
  au.close.late<-au.close.late[-length(au.close.late)]
  au.close.late<-log(au.close.late)
  #length(au.close.late)
  au.open<-log(au.max[,6])
  au.price[,1]<-abs(au.open-au.close.late)
  #View(au.price)
  #au.price[,1]<-abs(diff(log(au.price[,1])))
  #View(au.price)
  au.quaterspoints<-endpoints(au.price, on='quarters', k=1)
  au.quaterspoints<-au.quaterspoints[1:(length(au.quaterspoints)-1)]
  au.volatility<-c(1:(length(au.quaterspoints)-1))
  au.voi<-c(1:(length(au.quaterspoints)-1))
  au.oni<-c(1:(length(au.quaterspoints)-1))
  au.il1<-c(1:(length(au.quaterspoints)-1))
  au.il2<-c(1:(length(au.quaterspoints)-1))
  au.il3<-c(1:(length(au.quaterspoints)-1))
  au.volume.mean<-c(1:(length(au.quaterspoints)-1))
  au.amount.mean<-c(1:(length(au.quaterspoints)-1))
  for (i in (1:(length(au.quaterspoints)-1))) {
    #i=1
    au.volume.mean[i]<-mean(au.price[c((au.quaterspoints[i]+1):au.quaterspoints[i+1]),6])
    au.amount.mean[i]<-mean(au.price[c((au.quaterspoints[i]+1):au.quaterspoints[i+1]),7])
    au.pricequarter<-au.price[c((au.quaterspoints[i]+1):au.quaterspoints[i+1]),4]
    au.volatility.quarter<-sqrt(63) * sd(diff(log(au.pricequarter))[-1])
    au.volatility[i]<-au.volatility.quarter*100
    
    au.high<-na.omit(au.price[c((au.quaterspoints[i]+1):au.quaterspoints[i+1]),2])
    au.low<-na.omit(au.price[c((au.quaterspoints[i]+1):au.quaterspoints[i+1]),3])
    au.mean<-mean(mean(au.high),mean(au.low))
    au.major.volume<-sum(au.price[c((au.quaterspoints[i]+1):au.quaterspoints[i+1]),5])
    au.major.hold<-sum(au.price[c((au.quaterspoints[i]+1):au.quaterspoints[i+1]),6])
    au.voi[i]<-au.mean*au.major.volume/au.major.hold/(au.volatility.quarter)
    
    if(i==1){au.oni[i]<-mean(na.omit(au.price[c((au.quaterspoints[i]+1):au.quaterspoints[i+1]),1][-1]))}
    else{au.oni[i]<-mean(na.omit(au.price[c((au.quaterspoints[i]+1):au.quaterspoints[i+1]),1]))}
    
    au.il1[i]<-mean(na.omit(au.il1.daily[c((au.quaterspoints[i]+1):au.quaterspoints[i+1])]))
    if(j==1){au.il1[i]<-au.il1[i]*200}
    au.il2[i]<-mean(na.omit(au.il2.daily[c((au.quaterspoints[i]+1):au.quaterspoints[i+1])]))
    au.il3[i]<-(sum(au.price[c((au.quaterspoints[i]+1):au.quaterspoints[i+1]),5]))/
      (sum(au.price[c((au.quaterspoints[i]+1):au.quaterspoints[i+1]),6]))
  }
  au.volatility<-au.volatility[1:(length(au.volatility)-1)]
  au.volatility<-append(au.volatility,NA,after = 0)
  #View(au.volatility)
  au.result<-ts(cbind(au.volatility,au.voi,au.oni,au.il1,au.il2,au.il3,au.volume.mean,au.amount.mean),frequency = 4,end = c(2016,2))
  au.result<-as.timeSeries(au.result)
  colnames(au.result)<-c("vol","voi","oni","il1","il2","il3","volume mean","amount mean")
  #View(au.result)
  write.xlsx2(au.result, paste0('C:\\UM\\R\\calGARCH\\output\\futures voo\\',au.file,'/vl',au.file,'.xls'), col.names = TRUE, row.names = TRUE)
  
}
