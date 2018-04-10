#compute the target period for real apps

setwd("~/ChangjiangGou/Workflows/code/real_apps")
rm(list=ls())

#MPSoC settings
Smin<-66
Smax<-1.2*1024
beta<-1000#communication time is trivial

#coefficient for target period
k<-seq(0.05,0.95,0.01)

appslist<-read.table("realapps.txt")
apps<-apply(appslist,1,read.table)

TargetPeriod<-function(app){
  TargetP<-max(app$V1)/Smax+k*(max(app$V1)/Smin+max(app$V1)/Smax-max(app$V1)/Smax)
}

targetPriod<-lapply(apps,TargetPeriod)
output.TP<-matrix(unlist(targetPriod),nrow = length(apps),ncol = length(k),byrow = TRUE)
write.table(output.TP,"targetPeriod.txt",quote=FALSE,row.names = FALSE,col.names = FALSE)

commutime<-lapply(apps,function(x){x$V2/beta})
unlist(lapply(commutime,max))
apply(output.TP,1,min)

#for(i in 1:length(apps)){
  #app<-read.table(as.character(appslist[i,1]))
  #app$V2<-app$V2/beta
  #write.table(app,as.character(appslist[i,1]),quote = FALSE,row.names = FALSE,col.names = FALSE)
#}

Pp<-matrix(0.01,nrow = length(apps),ncol = length(k))
write.table(Pp,"PT01.txt",quote=FALSE,row.names = FALSE,col.names = FALSE)

