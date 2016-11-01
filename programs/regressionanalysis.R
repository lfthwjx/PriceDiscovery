library(xlsx)
library(xts)
library(timeSeries)
setwd("C:\\UM\\R\\calGARCH\\output")
auf.path<-'./futures voo'
mu.path<-'./uncertainty'
dire.names<-list.dirs(auf.path,full.names = FALSE)[-1]
me.names<-list.files(mu.path)
lr.model<-list()
for (j in (1:length(dire.names))) {
  j=1
  auf.file<-dire.names[j]
  auf<-read.xlsx2(paste0(auf.path,'/',auf.file,'/v',auf.file,'.xls'), sheetIndex=1,startRow = 1, header=T)
  aui<-read.xlsx2(paste0('./price discovery/',auf.file,'.xlsx'), sheetIndex=1,startRow = 1, colIndex = c(1:9), header=T)
  aui.futures<-aui[,c(1,7)]
  aufi<-merge(auf,aui.futures)
  for (i in (1:length(me.names))) {
    i=1
    mu.file<-me.names[i]
    mu.cpi<-read.xlsx2(paste0(mu.path,'/',mu.file),startRow = 1,colIndex = c(1:2),sheetIndex = 1, header=T)
    #mu.cpi[,2]<-as.numeric(levels(mu.cpi[,2])[mu.cpi[,2]])  
    #mu.cpi<-ts(mu.cpi[,2],frequency = 4,end = c(2016,2))
    #mu.cpi<-as.timeSeries(mu.cpi)
    #write.xlsx2(mu.cpi,paste0(mu.path,'/',mu.file))
    #View(mu.cpi)
    au.cpi.all<-merge(mu.cpi,aufi)
    View(au.cpi.all)
    au.cpi.all[,1]<-as.Date(levels(au.cpi.all[,1])[au.cpi.all[,1]])
    au.cpi.all[,2]<-as.numeric(levels(au.cpi.all[,2])[au.cpi.all[,2]])
    au.cpi.all[,3]<-as.numeric(levels(au.cpi.all[,3])[au.cpi.all[,3]])
    au.cpi.all[,4]<-as.numeric(levels(au.cpi.all[,4])[au.cpi.all[,4]])
    au.cpi.all[,5]<-as.numeric(levels(au.cpi.all[,5])[au.cpi.all[,5]])
    au.cpi.all[,6]<-as.numeric(levels(au.cpi.all[,6])[au.cpi.all[,6]])
    au.cpi.all[,7]<-as.numeric(levels(au.cpi.all[,7])[au.cpi.all[,7]])
    au.cpi.all[,8]<-as.numeric(levels(au.cpi.all[,8])[au.cpi.all[,8]])
    au.cpi.all[,9]<-as.numeric(levels(au.cpi.all[,9])[au.cpi.all[,9]])
    au.cpi.all<-cbind(au.cpi.all,au.cpi.all[,2]*au.cpi.all[,4])
    #View(au.cpi.all)  
    colnames(au.cpi.all)<-c("date","mu","vol","voi","oi","il1","il2","il3","is","mu*voi")
    cor(au.cpi.all[,(2:10)][-3][-3])
    model<-lm((is~mu+mu*voi+vol+il2),data = au.cpi.all)
    #summary(model)
    au.cpi.sum<-summary(model)
    lr.model[[((j-1)*8+i)]]<-au.cpi.sum
    #str(au.cpi.sum)
    }
}
#lr.model[[1]]
lr.df = as.data.frame(do.call(rbind, lr.model))
#View(lr.df[,4])
#write.xlsx2(lr.model, 'D:\\UM\\R\\calGARCH\\output\\lrsummary.xls', col.names = TRUE, row.names = TRUE)
