library(truncdist)
directory<-"~/ChangjiangGou/Workflows/code/random_apps3/"
setwd(directory)
#for generating random applications, the random variable is in range
oldfiles<-list.files()
file.remove(oldfiles)
rm(list=ls())

Smin<-66
Smax<-1.2*1024
npratio<-0.5
#npratio<-seq(0.01,1,0.01)
app.length<-floor(npratio*512)
#postfix<-"5p"
#simulation_times=3000
simulation_times=500
avarage_weight=20000
k<-seq(0.05,0.95,0.01)
#k=0.4
Lambda_zero<-10^(-6)
TildeD<-4

#--------------------------------------------------

filenames=seq(1:simulation_times*length(npratio))
Pb<-matrix(,nrow=simulation_times*length(npratio),ncol = length(k))
i<-1; j<-1;n<-1;
for(j in 1:length(npratio)){
  i=1
  while(i<=simulation_times){
    weight<-rtrunc(app.length[j],"norm",a=100,b=4000,mean=avarage_weight,sd=500)
    #weight<-rtrunc(app.length,"exp",a=2000,b=40000,rate=1/avarage_weight)
    Pbound<-max(weight)/Smax+k*(max(weight)/Smin+max(weight)/Smax-max(weight)/Smax)

    output<-rtrunc(app.length[j],"norm",a=0.0001*min(Pbound),b=min(Pbound),mean=0.001*min(Pbound))
    
    apps<-matrix(data = c(weight,output),nrow=app.length[j],ncol=2,byrow = FALSE)
    
    criteria1<-max(Lambda_zero*exp(TildeD)*weight/Smin)<=1e-2
    criteria2<-max(Lambda_zero*weight/Smax)<=1e-4
    criteria3<-min(Pbound)>=max(weight)/Smax
    
    if(criteria1&&criteria2&&criteria3){
      Pb[n,]<-Pbound
      filenames[n]<-paste("app",j,"_",i,".txt",sep="")
      write.table(apps,paste("app",j,"_",i,".txt",sep=""),quote=FALSE,row.names = FALSE,col.names = FALSE)
      i=i+1
      n=n+1
    }
  }
}

write.table(Pb,"TargetPeriod.txt",quote=FALSE,row.names = FALSE,col.names = FALSE)

Pp<-matrix(0.05,nrow = simulation_times*length(npratio),ncol = length(k))
write.table(Pp,"PT05.txt",quote=FALSE,row.names = FALSE,col.names = FALSE)

write.table(filenames,"random_apps.txt",quote=FALSE,row.names = FALSE,col.names = FALSE)
