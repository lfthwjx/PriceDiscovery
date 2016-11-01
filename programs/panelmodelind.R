library(xlsx)
library(xts)
library(timeSeries)
library(plm)
#library(car)
#library(dynlm)

setwd("D:\\UM\\R\\calGARCH\\output")
load("./.RData")
lr.model.arg<-list()
lr.arg.inter<-list()
lr.model.met<-list()
lr.met.inter<-list()

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
  
  year_ind<-grepl(toString(2004),cpi.met[,1])
  for (y in (2005:2016)) {
    year_sin<-grepl(toString(y),cpi.met[,1])
    year_ind<-cbind(year_ind,year_sin)
  }
  colnames(year_ind)<-c("y04","y05","y06","y07","y08","y09","y10","y11","y12","y13","y14","y15","y16")
  year_ind<-1*year_ind
  for (i in c(1:ncol(year_ind))) 
  {
    if(isTRUE(all.equal(max(year_ind[,i]),min(year_ind[,i])))==FALSE)
      {
      year_ind<-year_ind[,c(i:ncol(year_ind))]
      break
      }
  }
  cpi.met<-cbind(cpi.met,year_ind)
  
  met.name<-unique(cpi.met$future.name)
  name_ind<-(cpi.met$future.name==met.name[1])
  for (i in c(2:length(met.name))) {
    name_sin<-(cpi.met$future.name==met.name[i])
    name_ind<-cbind(name_ind,name_sin)
  }
  #met.name<-duplicated(cpi.met$future.name)
  name_ind<-1*name_ind
  colnames(name_ind)<-met.name
  cpi.met<-cbind(cpi.met,name_ind)
  
  return(cpi.met)
}

auf.path<-'./futures voo'
mu.path<-'./uncertainty'
dire.names<-list.dirs(auf.path,full.names = FALSE)[-1]
me.names<-list.files(mu.path)
for (i in (1:length(me.names))) {
  #i=1
  mu.file<-me.names[i]
  mu.cpi<-read.xlsx2(paste0(mu.path,'/',mu.file),startRow = 1,colIndex = c(1:2),sheetIndex = 1, header=T)
  
  for (j in (1:length(dire.names))) {
    #j=17
    auf.file<-dire.names[j]
    auf<-read.xlsx2(paste0(auf.path,'/',auf.file,'/v',auf.file,'.xls'), sheetIndex=1,startRow = 1, header=T)
    aui<-read.xlsx2(paste0('./price discovery/',auf.file,'.xlsx'), sheetIndex=1,startRow = 1, colIndex = c(1:9), header=T)
    aui.futures<-aui[,c(1,7)]
    aufi<-merge(auf,aui.futures)
    aufimu<-merge(mu.cpi,aufi)
    au.name<-rep(auf.file,nrow(aufimu))
    #View(au.name)
    aufimu<-cbind(aufimu,au.name)
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
  #View(cpi.met)
  form.arg<-is ~voi+il3+y04+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15+y16+
                baitang+bean1+caiyou+doupo+douyou+mianhua+qiangmai+yumi+zonglvyou
  form.arg<-is ~mu+voi+il3+
                baitang+bean1+caiyou+doupo+douyou+mianhua+qiangmai+yumi+zonglvyou
  model.arg<-lm(form.arg,data = cpi.arg)
  summary(model.arg)
  lr.model.arg[[i]]<-summary(model.arg)
  
  form.arg.inter<-is ~ mu*vol + mu*voi + mu*il3
  model.arg.inter<-lm(form.arg.inter,data = cpi.arg)
  lr.arg.inter[[i]]<-summary(model.arg.inter)
  
  form.met<-is ~ voi + il3
  model.met<-lm(form.met,data = cpi.met)
  summary(model.met)
  lr.model.met[[i]]<-summary(model.met)
  
  form.met.inter<-is ~ mu*vol + mu*voi + mu*il1
  model.met.inter<-lm(form.met.inter,data = cpi.met)
  lr.met.inter[[i]]<-summary(model.met.inter)
}
#plot(model.arg)
for (i in (1:length(me.names))) {
  print(paste0("linear regression for met."," with ",me.names[i]))
  print(lr.model.met[[i]])
}

for (i in (1:length(me.names))) {
  print(paste0("linear regression for met. interactively"," with ",me.names[i]))
  print(lr.met.inter[[i]])
}

for (i in (1:length(me.names))) {
  print(paste0("linear regression for agr."," with ",me.names[i]))
  print(lr.model.arg[[i]])
}

for (i in (1:length(me.names))) {
  print(paste0("linear regression for agr. interactively"," with ",me.names[i]))
  print(lr.arg.inter[[i]])
}
write.xlsx2(cpi.arg,"./cpiarg.xlsx",col.names = TRUE)
#View(cpi.arg)
#cor(cpi.arg[,(2:11)])
#form.arg.late<-is~L(mu,1)+L(vol,1)+voi+il3
#model.arg.late<-dynlm(form.arg.late,data=cpi.arg)
#summary(model.arg.late)

#met.cpi.pl <- plm.data(cpi.met, index = c("future.name", "date"))
#View(met.cpi.pl)
#gr_pool <- plm(is ~ mu+vol+voi, data = met.cpi.pl,model = "pooling")

#met.cpi.pl<-pgmm(dynformula(form, list(0, 0, 0, 0)),
#                 data=cpi.met,index=c("future.name", "date"),
#                 effect = "twoways", model = "twosteps",
#                 gmm.inst = ~ is)