library(xlsx)
library(xts)
library(timeSeries)
library(plm)
setwd("D:\\UM\\R\\calGARCH\\output")

cleandata<-function(au.cpi.all){
  au.cpi.all[,1]<-as.Date(levels(au.cpi.all[,1])[au.cpi.all[,1]])
  au.cpi.all[,2]<-as.numeric(levels(au.cpi.all[,2])[au.cpi.all[,2]])
  au.cpi.all[,3]<-as.numeric(levels(au.cpi.all[,3])[au.cpi.all[,3]])
  au.cpi.all[,4]<-as.numeric(levels(au.cpi.all[,4])[au.cpi.all[,4]])
  au.cpi.all[,5]<-as.numeric(levels(au.cpi.all[,5])[au.cpi.all[,5]])
  au.cpi.all[,6]<-as.numeric(levels(au.cpi.all[,6])[au.cpi.all[,6]])
  #au.cpi.all<-cbind(au.cpi.all,au.cpi.all[,2]*au.cpi.all[,4])
  colnames(au.cpi.all)<-c("date","mu","vol","voi","oi","is","future.name")}

auf.path<-'./futures voo'
mu.path<-'./uncertainty'
dire.names<-list.dirs(auf.path,full.names = FALSE)[-1]
me.names<-list.files(mu.path)
for (i in (1:length(me.names))) {
  i=1
  mu.file<-me.names[i]
  mu.cpi<-read.xlsx2(paste0(mu.path,'/',mu.file),startRow = 1,colIndex = c(1:2),sheetIndex = 1, header=T)
  for (j in (1:length(dire.names))) {
  #j=1
  auf.file<-dire.names[j]
  auf<-read.xlsx2(paste0(auf.path,'/',auf.file,'/v',auf.file,'.xls'), sheetIndex=1,startRow = 1, header=T)
  aui<-read.xlsx2(paste0('./price discovery/',auf.file,'.xlsx'), sheetIndex=1,startRow = 1, colIndex = c(1:9), header=T)
  aui.futures<-aui[,c(1,7)]
  aufi<-merge(auf,aui.futures)
  aufimu<-merge(mu.cpi,aufi)
  au.name<-rep(auf.file,nrow(aufimu))
  #View(au.name)
  aufimu<-cbind(aufimu,au.name)
  #View(aufimu)
  if(j==1||j==6||j==9||j==10||j==16){
    if(j==1){
      cpi.met<-aufimu
    }
    else{
      cpi.met<-rbind(cpi.met,aufimu)
    }
  }
  else if(j==12||14){
    if(j==12){cpi.pvc<-aufimu}
    else{cpi.xj<-aufimu}
  }
  else{
    if(j==2){
      cpi.arg<-aufimu
    }
    else{
      cpi.arg<-rbind(cpi.arg,aufimu)
    }
  }
  }
  
  View(cpi.met)
  #str(cpi.met)
  #cpi.met<-cleandata(cpi.met)
}

#met.cpi.pl <- plm.data(cpi.met, index = c("future.name", "date"))
#View(met.cpi.pl)
#gr_pool <- plm(is ~ mu+vol+voi, data = met.cpi.pl,model = "pooling")
cpi.met[,1]<-as.Date(levels(cpi.met[,1])[cpi.met[,1]])
cpi.met[,2]<-as.numeric(levels(cpi.met[,2])[cpi.met[,2]])
cpi.met[,3]<-as.numeric(levels(cpi.met[,3])[cpi.met[,3]])
cpi.met[,4]<-as.numeric(levels(cpi.met[,4])[cpi.met[,4]])
cpi.met[,5]<-as.numeric(levels(cpi.met[,5])[cpi.met[,5]])
cpi.met[,6]<-as.numeric(levels(cpi.met[,6])[cpi.met[,6]])
colnames(cpi.met)<-c("date","mu","vol","voi","oi","is","future.name")
View(cpi.met)
form<-is ~ mu + vol + voi
met.cpi.pl<-pgmm(dynformula(form, list(0, 0, 0, 0)),
                 data=cpi.met,index=c("future.name", "date"),
                 effect = "twoways", model = "twosteps",
                 gmm.inst = ~ is)