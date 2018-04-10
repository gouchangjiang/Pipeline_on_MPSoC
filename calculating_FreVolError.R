rm(list=ls())

#---for generating the voltage and corresponding frequency, y=k*x+b
smax=1.2*1024;
smin=66
k<-(smax-260)/(1.3-0.75)
b<-smax-(k*1.3)
voltage=seq(from=0.75,to=1.3,length.out=5)
voltage
frequency=k*voltage+b
frequency
normalized.speed<-frequency/smax
normalized.speed
#---finished

#---calculating the Error rate, lambda=10^-6, d=4 or d=5
lamb<-10^{-6}
d<-4
#d<-5
#d<-6

ErrorFun<-function(speed){
  lamb*exp(d*(smax-speed)/(smax-smin))
}

frequency<-c(smin,frequency)
ErrorFun(frequency)