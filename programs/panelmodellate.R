library(xlsx)
library(xts)
library(timeSeries)
library(plm)
setwd("C:\\UM\\R\\calGARCH\\output")

lr.model.arg<-list()
lr.model.met<-list()

cleandata<-function(cpi.met){
  cpi.met<-unname(cpi.met)
  cpi.met.num<-apply(cpi.met[,c(2:9)],2,as.numeric)
  cpi.met.num<-cbind(cpi.met.num,cpi.met.num[,1]*cpi.met.num[,3],cpi.met.num[,1]*cpi.met.num[,4])
  
  cpi.met<-cbind(cpi.met[,1],cpi.met.num,cpi.met[,10])
  colnames(cpi.met)<-c("date","mu","vol","voi","oi","il1","il2","il3","is","mu*voi","mu*oi","future.name")
  cpi.met<-as.data.frame(cpi.met)
  for (k in c(2:11)) {
    cpi.met[,k]<-as.numeric(levels(cpi.met[,k])[cpi.met[,k]])
  }
  return(cpi.met)
}

auf.path<-'./futures voo'
mu.path<-'./uncertainty'
dire.names<-list.dirs(auf.path,full.names = FALSE)[-1]
me.names<-list.files(mu.path)
for (i in (1:length(me.names))) {
  i=1
  mu.file<-me.names[i]
  mu.cpi<-read.xlsx2(paste0(mu.path,'/',mu.file),startRow = 1,colIndex = c(1:2),sheetIndex = 1, header=T)
  mu.cpi<-cbind(levels(mu.cpi$X.)[mu.cpi$X.][(2:nrow(mu.cpi))],levels(mu.cpi$TS.1)[mu.cpi$TS.1][(1:(nrow(mu.cpi)-1))])
  colnames(mu.cpi)<-c("X.","TS.1")
  for (j in (1:length(dire.names))) {
    #j=1
    auf.file<-dire.names[j]
    auf<-read.xlsx2(paste0(auf.path,'/',auf.file,'/vl',auf.file,'.xls'), sheetIndex=1,startRow = 1, header=T)
    aui<-read.xlsx2(paste0('./price discovery/',auf.file,'.xlsx'), sheetIndex=1,startRow = 1, colIndex = c(1:9), header=T)
    aui.futures<-aui[,c(1,7)]
    aufi<-merge(auf,aui.futures)
    aufimu<-merge(mu.cpi,aufi)
    au.name<-rep(auf.file,nrow(aufimu))
    #View(au.name)
    aufimu<-cbind(aufimu,au.name)
    #View(aufimu)
    aufimu<-aufimu[-1,]
    aufimu<-as.matrix(aufimu)
    #View(aufimu)
    #str(aufimu)
    if(j==1||j==6||j==9||j==10||j==16)
    {
      if(j==1)
      {
        cpi.met<-aufimu
      }
      else
      {
        cpi.met<-rbind(cpi.met,aufimu)
      }
    }
    else if(j==4||j==12||j==14)
    {
      if(j==12)
      {
        cpi.pvc<-aufimu
      }
      else if(j==14)
      {
        cpi.xj<-aufimu
      } 
      else
      {
        cpi.bean2<-aufimu
      }
    }
    else
    {
      if(j==2)
      {
        cpi.arg<-aufimu
      }
      else
      {
        cpi.arg<-rbind(cpi.arg,aufimu)
      }
    }
  }
  cpi.met<-cleandata(cpi.met = cpi.met)
  cpi.arg<-cleandata(cpi.met = cpi.arg)
  #View(cpi.arg)
  #cor(cpi.arg[,(2:11)])
  
  form.arg<-is ~ mu + vol +voi+ il3
  model.arg<-lm(form.arg,data = cpi.arg)
  #summary(model.arg)
  lr.model.arg[[i]]<-summary(model.arg)
  
  form.met<-is ~ mu + vol +voi+ il1
  model.met<-lm(form.met,data = cpi.met)
  lr.model.met[[i]]<-summary(model.met)
}

#met.cpi.pl <- plm.data(cpi.met, index = c("future.name", "date"))
#View(met.cpi.pl)
#gr_pool <- plm(is ~ mu+vol+voi, data = met.cpi.pl,model = "pooling")

met.cpi.pl<-pgmm(dynformula(form, list(0, 0, 0, 0)),
                 data=cpi.met,index=c("future.name", "date"),
                 effect = "twoways", model = "twosteps",
                 gmm.inst = ~ is)