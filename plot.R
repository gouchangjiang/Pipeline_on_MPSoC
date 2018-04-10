setwd("~/ChangjiangGou/Workflows/code/result")
library(ggplot2)
library(reshape2)
library(tikzDevice)
library(plyr)

#for visulasiation of the result
#-------------------Expected Period to Pbound,------------------------------
#-------------_____________---------------____________________-------------_____________
rm(list=ls())
str1<-"disc"
#str1<-"cont"

resultfile<-paste("result_norm_5p_",str1,"_pt5.txt",sep = "")
result<-read.table(resultfile,header = TRUE)
result<-result[1:1638000,]

result$Heuristics<-revalue(result$Heuristics,c(BestE="BestEnergy",DupAll="DuplicateAll",MaxS="MaxSpeed"))

result$ExpectedP<-as.numeric(as.character(result$ExpectedP))
result$ActualProba<-as.numeric(as.character(result$ActualProba))
result.summary<-ddply(result,c("K","Heuristics"),summarise,minEP=min(ExpectedP),
                      ExpectedPeriod=mean(ExpectedP),maxEP=max(ExpectedP),
                      minPro=min(ActualProba),ActualProbability=mean(ActualProba),
                      maxPro=max(ActualProba)
                      )
temp<-ddply(result,"K",summarise,avaPT=mean(Pbound),maxPT=max(Pbound),minPT=min(Pbound))
result.summary<-merge(result.summary,temp)
temp<-ddply(result,"K",summarise,avaProbaT=mean(ProbaT))
result.summary<-merge(result.summary,temp)

#display.brewer.all()
#colors()

hues = seq(15, 375, length = length(levels(result.summary$Heuristics)) + 1)
hcl(h = hues, l = 65, c = 100)[1:length(levels(result.summary$Heuristics))]
levels(result.summary$Heuristics)
cb_palette <- c(BestEnergy="#F8766D", BestTrade="#C49A00", Closer="#53B400", 
                DuplicateAll="#00C094", Failure="#00B6EB", MaxSpeed="#A58AFF", 
                Threshold="#FB61D7")#,"009E73")

outputfigure<-paste("/Users/changjiang/ChangjiangGou/Workflows/EP_",str1,"_5p.tex",sep = "")
#tikz(outputfigure,width = 6,height = 6)
ggplot(result.summary,aes(x=K,y=ExpectedPeriod,colour=Heuristics))+
  geom_ribbon(aes(ymin=minEP, ymax=maxEP,colour=Heuristics),alpha=0.1)+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymin=minPT,ymax=maxPT),linetype="dotted",colour="black",alpha=0.1)+
  geom_line(aes(y=avaPT),colour="black",linetype="dotted",size=1.5)+
  coord_cartesian(y=c(0,60))+
  labs(colour="Heuristics",x="kappa",y="Expected Period")+
  scale_colour_manual(values = cb_palette)+
  theme(legend.position=c(0.98,0.13),legend.justification = c(1,0))+
  theme(legend.background = element_rect(colour = "black"),legend.title = element_text(size = 18),
        legend.text = element_text(size=16))+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 20))
#ggsave("/Users/changjiang/ChangjiangGou/Thesis/Year2/fig/EP_cont_5p.pdf", width=9,height = 6,units = "in",dpi=300)#a4 21*29.7
dev.off()

outputfigure<-paste("/Users/changjiang/ChangjiangGou/Workflows/AP_",str1,"_5p.tex",sep="")
#tikz(outputfigure,width = 6,height = 6)
ggplot(result.summary,aes(x=K,y=ActualProbability,colour=Heuristics))+
  geom_ribbon(aes(ymin=minPro, ymax=maxPro,colour=Heuristics),alpha=0.1)+
  geom_line(size=1.5)+
  geom_line(aes(y=avaProbaT),colour="black",linetype="dotted",size=1.5)+
  #theme(axis.line = element_line(colour="black"))+
  coord_cartesian(y=c(0,0.1))+
  #coord_cartesian(y=c(0,0.3))+
  labs(colour="Heuristics",linetype="Target/Expected",x="kappa",y="Probability of exceeding the period bound")+
  scale_colour_manual(values = cb_palette)+
  guides(colour=FALSE)+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 20))
#ggsave("/Users/changjiang/ChangjiangGou/Thesis/Year2/fig/AP_cont_5p.pdf", width=9,height = 6,units = "in",dpi=300)#a4 21*29.7

dev.off()

result$Energy<-as.numeric(as.character(result$Energy))
energy.wide<-dcast(result,Tree_name+NPRatio+K~Heuristics,value.var = "Energy")
energy.wide$BestTrade<-energy.wide$BestTrade/energy.wide$BestEnergy
energy.wide$Closer<-energy.wide$Closer/energy.wide$BestEnergy
energy.wide$DuplicateAll<-energy.wide$DuplicateAll/energy.wide$BestEnergy
energy.wide$MaxSpeed<-energy.wide$MaxSpeed/energy.wide$BestEnergy
energy.wide$Threshold<-energy.wide$Threshold/energy.wide$BestEnergy
energy.long<-melt(energy.wide,id.vars=c("Tree_name","K","NPRatio"),variable.name="Heuristics",value.name = "NEnergy")
energy.long<-energy.long[!energy.long$Heuristics=="BestEnergy",]

energy.summary<-ddply(energy.long,c("K","Heuristics"),summarise,minE=min(NEnergy),
                      E=mean(NEnergy),maxE=max(NEnergy))

outputfigure<-paste("/Users/changjiang/ChangjiangGou/Workflows/E_",str1,"_5p.tex",sep = "")
#tikz(outputfigure,width = 6,height = 6)
#ytitle<-expression(paste("Energy cost normalised to ",bold(BestEnergy)))
ggplot(energy.summary,aes(x=K,y=E,colour=Heuristics))+
  geom_line(size=1.5)+
  coord_cartesian(y=c(0,45))+#215.4228 for MaxSpeed
  labs(colour="Heuristics",x="kappa",y="Energy cost normalised to \\textbf{BestEnergy}")+
  scale_colour_manual(values = cb_palette)+
  guides(colour=FALSE)+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 20))
#ggsave("/Users/changjiang/ChangjiangGou/Thesis/Year2/fig/E_cont_5p.pdf", width=9,height = 6,units = "in",dpi=300)#a4 21*29.7
dev.off()
#------------------------expected period with different chains' size----------------------------------
rm(list=ls())
str1<-"disc"
#str1<-"cont"

p<-512

resultfile<-paste("result_",str1,"_manyNPratios.txt",sep = "")
result<-read.table(resultfile,header = TRUE)

result<-result[1:1800000,]
result<-result[result$Energy!=-1,]
result$Heuristics<-revalue(result$Heuristics,c(BestE="BestEnergy",DupAll="DuplicateAll",MaxS="MaxSpeed"))
result$Energy<-as.numeric(as.character(result$Energy))
#result$Energy<-result$Energy/(p*result$NPRatio)

energy.wide<-dcast(result,Tree_name+NPRatio+K~Heuristics,value.var = "Energy")
energy.wide$BestEnergy<-as.numeric(energy.wide$BestEnergy)
energy.wide$BestTrade<-as.numeric(energy.wide$BestTrade)
energy.wide$Closer<-as.numeric(energy.wide$Closer)
energy.wide$DuplicateAll<-as.numeric(energy.wide$DuplicateAll)
energy.wide$MaxSpeed<-as.numeric(energy.wide$MaxSpeed)
energy.wide$Threshold<-as.numeric(energy.wide$Threshold)

energy.wide$BestTrade<-energy.wide$BestTrade/energy.wide$BestEnergy
energy.wide$Closer<-energy.wide$Closer/energy.wide$BestEnergy
energy.wide$DuplicateAll<-energy.wide$DuplicateAll/energy.wide$BestEnergy
energy.wide$MaxSpeed<-energy.wide$MaxSpeed/energy.wide$BestEnergy
energy.wide$Threshold<-energy.wide$Threshold/energy.wide$BestEnergy
energy.long<-melt(energy.wide,id.vars=c("Tree_name","K","NPRatio"),variable.name="Heuristics",value.name = "NEnergy")
energy.long<-subset(energy.long,Heuristics!="BestEnergy")

p<-512

energy.summary<-ddply(energy.long,c("NPRatio","Heuristics"),summarise,minE=min(NEnergy),
                      E=mean(NEnergy),maxE=max(NEnergy))
#mean(energy.long$NEnergy[which(energy.long$Heuristics=="MaxSpeed")]) #maxspeed is 215

outputfigure<-paste("/Users/changjiang/ChangjiangGou/Workflows/E_",str1,"_4k.tex",sep = "")
#outputfigure<-paste("/Users/changjiang/ChangjiangGou/Workflows/E_",str1,"_4k_eachnode.tex",sep = "")
#tikz(outputfigure,width = 6,height = 6)
#ytitle<-expression(paste("Mean energy cost\n(normalised to",bold(BestEnergy),")"))
ggplot(energy.summary,aes(x=NPRatio,y=E,colour=Heuristics))+
  geom_ribbon(aes(ymin=minE, ymax=maxE,colour=Heuristics),alpha=0.1)+
  geom_line(size=1.5)+
  #coord_cartesian(y=c(0,7))+
  coord_cartesian(y=c(5,25))+
  scale_colour_manual(values = cb_palette)+
  #guides(colour=FALSE)+
  labs(colour="Heuristics",x="Ratio of node to core",y="Energy cost normalized to \\textbf{BestEnergy}")+
  #labs(colour="Heuristics",x="Ratio of node to core",y="Mean node's energy cost\n(normalized to \\textbf{BestEnergy})")+
  theme(legend.position=c(0.98,0.4),legend.justification = c(1,0))+
  theme(legend.background = element_rect(colour = "black"),legend.title = element_text(size = 18),
        legend.text = element_text(size=16))+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 20))
  dev.off()
#ggsave("/Users/changjiang/ChangjiangGou/Thesis/Year2/fig/E_cont_4k.pdf",  width=9,height = 6,units = "in",dpi=300)#a4 21*29.7

result$ExpectedP<-as.numeric(as.character(result$ExpectedP))
result$ActualProba<-as.numeric(as.character(result$ActualProba))
Period.summary<-ddply(result,c("NPRatio","Heuristics"),summarise,minEP=min(ExpectedP),EP=mean(ExpectedP),maxEP=max(ExpectedP))
Proba.summary<-ddply(result,c("NPRatio","Heuristics"),summarise,minPt=min(ActualProba),Pt=mean(ActualProba),maxPt=max(ActualProba))

temp<-ddply(result,"NPRatio",summarise,avaP=mean(Pbound),maxPd=max(Pbound),minPd=min(Pbound))
Period.summary<-merge(Period.summary,temp)

outputfigure<-paste("/Users/changjiang/ChangjiangGou/Workflows/EP_NP_",str1,".tex",sep = "")
#tikz(outputfigure,width = 6,height = 6)
ggplot(Period.summary,aes(x=NPRatio,y=EP,colour=Heuristics))+
  geom_ribbon(aes(ymin=minEP, ymax=maxEP,colour=Heuristics),alpha=0.1)+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymin=minPd,ymax=maxPd),linetype="dotted",colour="black",alpha=0.1)+
  geom_line(aes(y=avaP),colour="black",linetype="dotted",size=1.5)+
  scale_colour_manual(values = cb_palette)+
  labs(colour="Heuristics",x="Ratio of node to core",y="Expected Period")+
  guides(colour=FALSE)+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 20))
dev.off()
#ggsave("/Users/changjiang/ChangjiangGou/Thesis/Year2/fig/EP_NP_cont.pdf", width=9, height=6, units="in",dpi=300)#a4 21*29.7

outputfigure<-paste("/Users/changjiang/ChangjiangGou/Workflows/AP_NP_",str1,".tex",sep = "")
#tikz(outputfigure,width = 6,height = 6)
ggplot(Proba.summary,aes(x=NPRatio,y=Pt,colour=Heuristics))+
  geom_ribbon(aes(ymin=minPt, ymax=maxPt,colour=Heuristics),alpha=0.1)+
  geom_line(size=1.5)+
  geom_line(aes(y=0.05),colour="black",linetype="dotted")+
  coord_cartesian(y=c(0,0.075))+#BestEnergy is always 1
  scale_colour_manual(values = cb_palette)+
  labs(colour="Heuristics",x="Ratio of node to core",y="Probability of exceeding the period bound")+
  guides(colour=FALSE)+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 20))
dev.off()
#ggsave("/Users/changjiang/ChangjiangGou/Thesis/Year2/fig/AP_NP_cont.pdf", width=9, height=6, units="in",dpi=300)#a4 21*29.7


#------------------------------------------on real apps----------------------------
rm(list=ls())
str1<-"disc"
#str1<-"cont"

resultfile<-paste("result_real_",str1,".txt",sep = "")
result<-read.table(resultfile,header = TRUE)
result<-result[1:4368,]

result$Heuristics<-revalue(result$Heuristics,c(BestE="BestEnergy",DupAll="DuplicateAll",MaxS="MaxSpeed"))

result$ExpectedP<-as.numeric(as.character(result$ExpectedP))
result$ActualProba<-as.numeric(as.character(result$ActualProba))
result.summary<-ddply(result,c("K","Heuristics"),summarise,minEP=min(ExpectedP),
                      ExpectedPeriod=mean(ExpectedP),maxEP=max(ExpectedP),
                      minPro=min(ActualProba),ActualProbability=mean(ActualProba),
                      maxPro=max(ActualProba)
)
temp<-ddply(result,"K",summarise,avaPT=mean(Pbound),maxPT=max(Pbound),minPT=min(Pbound))
result.summary<-merge(result.summary,temp)
temp<-ddply(result,"K",summarise,avaProbaT=mean(ProbaT))
result.summary<-merge(result.summary,temp)

outputfigure<-paste("/Users/changjiang/ChangjiangGou/Workflows/EP_real_",str1,".tex",sep="")
#tikz(outputfigure,width = 6,height = 6)
ggplot(result.summary,aes(x=K,y=ExpectedPeriod,colour=Heuristics))+
  #geom_ribbon(aes(ymin=minPT,ymax=maxPT,colour="Period bound"),alpha=0.1)+
  #geom_ribbon(aes(ymin=minEP, ymax=maxEP,colour=Heuristics),alpha=0.1)+
  geom_line(size=1.5)+
  geom_line(aes(y=avaPT),colour="black",linetype="dotted",size=1.5)+
  scale_colour_manual(values = cb_palette)+
  labs(colour="Heuristics",x="kappa",y="Expected Period")+
  #theme(legend.position=c(0.98,0.13),legend.justification = c(1,0))#+
  #guides(colour=FALSE)+
  theme(legend.position=c(0.4,0.6),legend.justification = c(1,0))+
  theme(legend.background = element_rect(colour = "black"),legend.title = element_text(size = 18),
        legend.text = element_text(size=16))+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 20))
dev.off()
#ggsave("/Users/changjiang/ChangjiangGou/Workflows/ExpectedP_disc_realapps.pdf", width=9,height = 6,units = "in",dpi=300)#a4 21*29.7

outputfigure<-paste("/Users/changjiang/ChangjiangGou/Workflows/AP_real_",str1,".tex",sep="")
#tikz(outputfigure,width = 6,height = 6)
ggplot(result.summary,aes(x=K,y=ActualProbability,colour=Heuristics))+
  geom_ribbon(aes(ymin=minPro, ymax=maxPro,colour=Heuristics),alpha=0.1)+
  geom_line(size=1.5)+
  geom_line(aes(y=avaProbaT),colour="black",linetype="dotted",size=1.5)+
  #coord_cartesian(ylim = c(0,0.03))+#BestEnergy is always 1
  coord_cartesian(ylim = c(0,0.16))+#BestEnergy is always 1
  scale_colour_manual(values = cb_palette)+
  labs(colour="Heuristics",linetype="Target/Expected",x="kappa",y="Probability of exceeding the period bound")+
  guides(colour=FALSE)+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 20))
dev.off()

#ggsave("/Users/changjiang/ChangjiangGou/Workflows/Probability_disc_realapps.pdf", width=9,height = 6,units = "in",dpi=300)#a4 21*29.7

result$Energy<-as.numeric(as.character(result$Energy))
energy.wide<-dcast(result,Tree_name+NPRatio+K~Heuristics,value.var = "Energy")
energy.wide$BestTrade<-energy.wide$BestTrade/energy.wide$BestEnergy
energy.wide$Closer<-energy.wide$Closer/energy.wide$BestEnergy
energy.wide$DuplicateAll<-energy.wide$DuplicateAll/energy.wide$BestEnergy
energy.wide$MaxSpeed<-energy.wide$MaxSpeed/energy.wide$BestEnergy
energy.wide$Threshold<-energy.wide$Threshold/energy.wide$BestEnergy
energy.long<-melt(energy.wide,id.vars=c("Tree_name","K","NPRatio"),variable.name="Heuristics",value.name = "NEnergy")
energy.long<-energy.long[!energy.long$Heuristics=="BestEnergy",]

energy.summary<-ddply(energy.long,c("K","Heuristics"),summarise,minE=min(NEnergy),
                      E=mean(NEnergy),maxE=max(NEnergy))

#mean(energy.long$NEnergy[which(energy.long$Heuristics=="MaxSpeed")]) #MaxSpeed is 254.0781

outputfigure<-paste("/Users/changjiang/ChangjiangGou/Workflows/E_real_",str1,".tex",sep="")
#tikz(outputfigure,width = 6,height = 6)
ggplot(energy.summary,aes(x=K,y=E,colour=Heuristics))+
  geom_line(size=1.5)+
  coord_cartesian(y=c(0,45))+#MaxSpeed is 254.0781
  scale_colour_manual(values = cb_palette)+
  labs(colour="Heuristics",x="kappa",y="Energy cost normalized to \\textbf{BestEnergy}")+
  guides(colour=FALSE)+
  theme(axis.title = element_text(size = 20),axis.text = element_text(size = 20))
dev.off()

#ggsave("/Users/changjiang/ChangjiangGou/Workflows/E_disc_realapps.pdf", width=9,height = 6,units = "in",dpi=300)#a4 21*29.7
