#Analysis to compare behavior and survival of Stegodyphus dumicola colonies 
#On fences and trees, in field and in the greenhouse

#load data
#colony level metrics
surv<-read.csv("colonymetrics.csv")
survsub=subset(surv, surv$Survival=="Yes")
survret=ddply(surv, c("Size", "Treatment"), summarize, ret=mean(NumRetreats, na.rm=TRUE), retse=sd(NumRetreats, na.rm=TRUE)/length(NumRetreats))

#field measurements of behavior
field<-read.csv("fielddata.csv")
#greenhouse measurements of behavior
gh<-read.csv("labdata.csv")

#to check behavior pre-deployment
ghpre=subset(gh, gh$Situation=="Cup"|gh$Situation=="Pre")
ghpre2=ddply(ghpre, c("ColonyID", "Treatment"), summarize, lat=mean(Latency, na.rm=TRUE), num=mean(Number, na.rm=TRUE))

#subset to only the trials, and the non-fused colonies
ghsub=subset(gh, gh$Situation!="Cup"&gh$Situation!="Pre"&gh$Fused=="0")
ghsub$Batch=as.factor(ghsub$Batch)
#to count number of fused colonies, i.e. experiencing immigration/emigration
ghfusion=ddply(gh, c("ColonyID", "Treatment"), summarize, fused=mean(Fused))
for (i in 1:nrow(ghfusion)){
  ghfusion$Fused[i]=if (ghfusion$fused[i]=="0") {"No"} else {"Yes"}
}

#load packages
library(ggplot2)
library(plyr)


#checking behavior pre-deployment
#in the field data
t.test(surv$AvgLatency~surv$Treatment, var.equal=TRUE)
t.test(surv$AvgNum~surv$Treatment, var.equal=TRUE)

#in the greenhouse data
t.test(ghpre2$lat~ghpre2$Treatment, var.equal=TRUE)
t.test(ghpre2$num~ghpre2$Treatment, var.equal=TRUE)

#no significant differences in colony behavior between treatments pre-deployment 

#Figures
#ONLY run this code when generating plots, will mess up comparisons/differences below
#field$ColonySize=factor(field$ColonySize, levels(field$ColonySize)[c(2,1)])
#surv$Size=factor(surv$Size, levels(surv$Size)[c(2,1)])


plot=ggplot(field)
#Figure 2a
plot+theme_classic(15)+geom_boxplot(aes(x=ColonySize, y=Latency, fill=Treatment))+
  scale_fill_manual(values=c("#E5BA52", "#00A757"))+xlab("Colony Size")+ylab("Time to Attack (in s)")+
  labs(fill="Spatial Context\nTreatment")
#Figure 2b
plot+theme_classic(15)+geom_boxplot(aes(x=ColonySize, y=NumAttack, fill=Treatment))+
  scale_fill_manual(values=c("#E5BA52", "#00A757"))+xlab("Colony Size")+ylab("Number of Attackers")+
  labs(fill="Spatial Context\nTreatment")


plot2=ggplot(surv)
#Figure3a--Prey in web
plot2+theme_classic(15)+geom_boxplot(aes(x=Treatment, y=PropDaysPrey, fill=Treatment))+theme(legend.position="NONE")+
  scale_fill_manual(values=c("#E5BA52", "#00A757"))+xlab("Treatment")+ylab("Proportion of Days\nwith Prey in Capture Web")
#Figure3c--colony survival
plot2+ theme_classic(15)+geom_bar(aes(x=Survival, y=..count.., fill=Treatment))+
scale_fill_manual(values=c("#E5BA52", "#00A757"))+xlab("Colony Persistence")+ylab("Number of Colonies")+labs(fill="Spatial Context\nTreatment")
#Figure 3b--number of individuals remaining  
plot4=ggplot(survsub)
plot4+theme_classic(15)+geom_boxplot(aes(x=Treatment, y=(FinalNumSpiders/InitialNumSpiders), fill=Treatment))+theme(legend.position="NONE")+
  scale_fill_manual(values=c("#E5BA52", "#00A757"))+xlab("Treatment")+ylab("Proportion of Spiders Remaining")
  
#Figure4a
plot6=ggplot(survret)
plot6+theme_classic(15)+geom_bar(aes(Size, ret, fill=Treatment), position="dodge", stat="identity")+
  scale_fill_manual(values=c("#E5BA52", "#00A757"))+xlab("Colony Size")+ylab("Number of Retreat Nests")+
  geom_errorbar(aes(x=c(0.75, 1.25, 1.75, 2.25), ymin=ret-retse, ymax=ret+retse), width=0.1)

#greenhouse 
#Figure2a
plot3=ggplot(ghsub)
plot3+theme_classic(15)+geom_boxplot(aes(x=Treatment, y=Latency, fill=Treatment))+
  scale_fill_manual(values=c("#E5BA52", "#00A757"))+xlab("Treatment")+ylab("Time to Attack (in s)")+
  labs(fill="Spatial Context\nTreatment")
#Figure2b
plot3+theme_classic(15)+geom_boxplot(aes(x=Treatment, y=Number, fill=Treatment))+
  scale_fill_manual(values=c("#E5BA52", "#00A757"))+xlab("Treatment")+ylab("Number of Attackers")+
  labs(fill="Spatial Context\nTreatment")

#Figure4b
plot7=ggplot(ghfusion)
plot7+theme_classic(15)+geom_bar(aes(x=Fused, y=..count.., fill=Treatment))+
scale_fill_manual(values=c("#E5BA52", "#00A757"))+xlab("Emigration/Immigration")+ylab("Number of Colonies")+
  labs(fill="Spatial Context\nTreatment")

#STATISTICAL TESTS
#colony level survival
table(surv$Size, surv$Survival)
table(surv$Treatment, surv$Survival)
fisher.test(as.matrix(table(surv$Treatment, surv$Survival)))
fisher.test(as.matrix(table(surv$Size, surv$Survival)))

#not reported, no effect of colony size on survival, within tree colonies 
survtree=subset(surv, surv$Treatment=="Tree")
fisher.test(as.matrix(table(survtree$Size, survtree$Survival)))

#in greenhouse, comparing colony fusion
ghfusion=ddply(gh, c("ColonyID", "Treatment"), summarize, fused=mean(Fused))
table(ghfusion$Treatment, ghfusion$fused)
fisher.test(as.matrix(table(ghfusion$Treatment, ghfusion$fused)))


#all other analyses with randomization instead of using parametric tests
N=10000
set.seed(123)
#first, field data: latency and number of attackers

#with interaction
dat=ddply(field, c("Treatment", "ColonySize"), summarize, lat=mean(Latency, na.rm=TRUE), num=mean(NumAttack, na.rm=TRUE))
#vectors of differences LF-SF, LT-ST, LF-LT, SF-ST; lat then num
fieldlatnum<-c((dat[1,3]-dat[2,3]), (dat[3,3]-dat[4,3]), (dat[1,3]-dat[3,3]), (dat[2,3]-dat[4,3]), 
               (dat[1,4]-dat[2,4]), (dat[3,4]-dat[4,4]), (dat[1,4]-dat[3,4]), (dat[2,4]-dat[4,4]))
int1=c((fieldlatnum[1]-fieldlatnum[2]), (fieldlatnum[3]-fieldlatnum[4]), (fieldlatnum[5]-fieldlatnum[6]), (fieldlatnum[7]-fieldlatnum[8]))
#randomizing within trial week

templatnum=matrix(0, N, 8)
for (i in 1:N){
  temp=ddply(field, "Trial", transform, size=sample(ColonySize, length(ColonySize), replace=FALSE), treatment=sample(Treatment, length(Treatment), replace=FALSE))
  tempdat=ddply(temp, c("treatment", "size"), summarize, lat=mean(Latency, na.rm=TRUE), num=mean(NumAttack, na.rm=TRUE))
  templatnum[i,]<-c((tempdat[1,3]-tempdat[2,3]), (tempdat[3,3]-tempdat[4,3]), (tempdat[1,3]-tempdat[3,3]), (tempdat[2,3]-tempdat[4,3]), 
                    (tempdat[1,4]-tempdat[2,4]),(tempdat[3,4]-tempdat[4,4]), (tempdat[1,4]-tempdat[3,4]), (tempdat[2,4]-tempdat[4,4]))
  rm(temp, tempdat)
}

tempint1=cbind(templatnum[,1]-templatnum[,2], templatnum[,3]-templatnum[,4], templatnum[,5]-templatnum[,6], templatnum[,7]-templatnum[,8])


pmat=matrix(0, N, 8)
pint1=matrix(0, N, 4)

for (i in 1:N){
  for (j in 1:8){
    pmat[i,j]=if (abs(templatnum[i,j])<abs(fieldlatnum[j])) {0} else {1}
  }
  for (j in 1:4){
    pint1[i,j]=if (abs(tempint1[i,j])<abs(int1[j])) {0} else {1}
  }
}

pfield=colSums(pmat)/N
pfieldint=colSums(pint1)/N
names(pfield)=names(fieldlatnum)=c("latLF-SF", "latLT-ST", "latLF-LT", "latSF-ST", "numLF-SF", "numLT-ST", "numLF-LT", "numSF-ST")



#significant interaction between colony size and fence/tree for number of attackers but not latency.
#redoing randomization holding colony size constant

templatnum2=matrix(0, N, 2)
dat2=ddply(field, "Treatment", summarize, lat=mean(Latency, na.rm=TRUE), num=mean(NumAttack, na.rm=TRUE))
fieldlatnum2=c((dat2[1, 2]-dat2[2,2]), (dat2[1,3]-dat2[2,3]))

for (i in 1:N){
  temp=ddply(field, c("Trial", "ColonySize"), transform, treatment=sample(Treatment, length(Treatment), replace=FALSE))
  tempdat=ddply(temp, c("treatment"), summarize, lat=mean(Latency, na.rm=TRUE), num=mean(NumAttack, na.rm=TRUE))
  templatnum2[i,]<-c((tempdat[1,2]-tempdat[2,2]),
                    (tempdat[1,3]-tempdat[2,3]))
  rm(temp, tempdat)
}

pmat2=matrix(0, N, 2)

for (i in 1:N){
  for (j in 1:2){
    pmat2[i,j]=if (abs(templatnum2[i,j])<abs(fieldlatnum2[j])) {0} else {1}
  }
}

pfield2=colSums(pmat2)/N
names(pfield2)=names(fieldlatnum2)=c("latF-T", "numF-T")

#and then holding treatment constant
dat3=ddply(field, "ColonySize", summarize, lat=mean(Latency, na.rm=TRUE), num=mean(NumAttack, na.rm=TRUE))
fieldlatnum3=c((dat3[1, 2]-dat3[2,2]), (dat3[1,3]-dat3[2,3]))
templatnum3=matrix(0, N, 2)

for (i in 1:N){
  temp=ddply(field, c("Trial", "Treatment"), transform, size=sample(ColonySize, length(ColonySize), replace=FALSE))
  tempdat=ddply(temp, c("size"), summarize, lat=mean(Latency, na.rm=TRUE), num=mean(NumAttack, na.rm=TRUE))
  templatnum3[i,]<-c((tempdat[1,2]-tempdat[2,2]),
                     (tempdat[1,3]-tempdat[2,3]))
  rm(temp, tempdat)
}

pmat3=matrix(0, N, 2)

for (i in 1:N){
  for (j in 1:2){
    pmat3[i,j]=if (abs(templatnum3[i,j])<abs(fieldlatnum3[j])) {0} else {1}
  }
}

pfield3=colSums(pmat3)/N
names(pfield3)=names(fieldlatnum3)=c("latL-S", "numL-S")

#next, colony level metrics: number of retreats, proportion of days with prey, and final proportion of spiders
dat4=ddply(surv, c("Treatment", "Size"), summarize, ret=mean(NumRetreats, na.rm=TRUE), prop=mean(PropDaysPrey), fin=mean(FinalNumSpiders/InitialNumSpiders, na.rm=TRUE))
#vectors of differences LF-SF, LT-ST, LF-LT, SF-ST; ret, then prop, then fin num
fieldretpropfin<-c((dat4[1,3]-dat4[2,3]), (dat4[3,3]-dat4[4,3]), (dat4[1,3]-dat4[3,3]), (dat4[2,3]-dat4[4,3]), 
                   (dat4[1,4]-dat4[2,4]), (dat4[3,4]-dat4[4,4]), (dat4[1,4]-dat4[3,4]), (dat4[2,4]-dat4[4,4]),
                   (dat4[1,5]-dat4[2,5]), (dat4[3,5]-dat4[4,5]), (dat4[1,5]-dat4[3,5]), (dat4[2,5]-dat4[4,5]))
int2=c((fieldretpropfin[1]-fieldretpropfin[2]), (fieldretpropfin[3]-fieldretpropfin[4]), 
       (fieldretpropfin[5]-fieldretpropfin[6]), (fieldretpropfin[7]-fieldretpropfin[8]),
       (fieldretpropfin[9]-fieldretpropfin[10]), (fieldretpropfin[11]-fieldretpropfin[12]))

tempretpropfin=matrix(0, N, 12)
for (i in 1:N){
  temp=surv
  temp$size=sample(temp$Size, length(temp$Size), replace=FALSE)
  temp$treatment=sample(temp$Treatment, length(temp$Treatment), replace=FALSE)
  tempdat=ddply(temp, c("treatment", "size"), summarize, ret=mean(NumRetreats, na.rm=TRUE), prop=mean(PropDaysPrey), fin=mean(FinalNumSpiders/InitialNumSpiders, na.rm=TRUE))
  tempretpropfin[i,]<-c((tempdat[1,3]-tempdat[2,3]), (tempdat[3,3]-tempdat[4,3]), (tempdat[1,3]-tempdat[3,3]), (tempdat[2,3]-tempdat[4,3]), 
                    (tempdat[1,4]-tempdat[2,4]),(tempdat[3,4]-tempdat[4,4]), (tempdat[1,4]-tempdat[3,4]), (tempdat[2,4]-tempdat[4,4]),
                    (tempdat[1,5]-tempdat[2,5]),(tempdat[3,5]-tempdat[4,5]), (tempdat[1,5]-tempdat[3,5]), (tempdat[2,5]-tempdat[4,5]))
  rm(temp, tempdat)
}

tempint2=cbind(tempretpropfin[,1]-tempretpropfin[,2], tempretpropfin[,3]-tempretpropfin[,4], tempretpropfin[,5]-tempretpropfin[,6], 
               tempretpropfin[,7]-tempretpropfin[,8],tempretpropfin[,9]-tempretpropfin[,10],  tempretpropfin[,11]-tempretpropfin[,12])


pmat4=matrix(0, N, 12)
pint2=matrix(0, N, 6)
for (i in 1:N){
  for (j in 1:12){
    pmat4[i,j]=if (abs(tempretpropfin[i,j])<abs(fieldretpropfin[j])) {0} else {1}
  }
  for (j in 1:6){
    pint2[i,j]=if (abs(tempint2[i,j])<abs(int2[j])) {0} else {1}
  }
}

psurv=colSums(pmat4)/N
psurvint=colSums(pint2)/N
names(psurv)=names(fieldretpropfin)=c("retLF-SF", "retLT-ST", "retLF-LT", "retSF-ST", "propLF-SF", "propLT-ST", "propLF-LT", "propSF-ST", "finLF-SF", "finLT-ST", "finLF-LT", "finSF-ST")

#holding colony size constant
dat5=ddply(surv, "Treatment", summarize, ret=mean(NumRetreats, na.rm=TRUE), prop=mean(PropDaysPrey), fin=mean(FinalNumSpiders/InitialNumSpiders, na.rm=TRUE))
#vectors of differences F-T, ret then prop then fin num
fieldretpropfin2<-c((dat5[1,2]-dat5[2,2]),
                    (dat5[1,3]-dat5[2,3]),
                    (dat5[1,4]-dat5[2,4]))

tempretpropfin2=matrix(0, N, 3)
for (i in 1:N){
  temp=ddply(surv, "Size", transform, treatment=sample(Treatment, length(Treatment), replace=FALSE))
  tempdat=ddply(temp, "treatment", summarize, ret=mean(NumRetreats, na.rm=TRUE), prop=mean(PropDaysPrey), fin=mean(FinalNumSpiders/InitialNumSpiders, na.rm=TRUE))
  tempretpropfin2[i,]<-c((tempdat[1,2]-tempdat[2,2]),
                        (tempdat[1,3]-tempdat[2,3]),
                        (tempdat[1,4]-tempdat[2,4]))
  rm(temp, tempdat)
}

pmat5=matrix(0, N, 3)

for (i in 1:N){
  for (j in 1:3){
    pmat5[i,j]=if (abs(tempretpropfin2[i,j])<abs(fieldretpropfin2[j])) {0} else {1}
  }
}

psurv2=colSums(pmat5)/N
names(psurv2)=names(fieldretpropfin2)=c("retF-T", "propF-T", "finF-T")

#holding treatment constant 
dat6=ddply(surv, "Size", summarize, ret=mean(NumRetreats, na.rm=TRUE), prop=mean(PropDaysPrey), fin=mean(FinalNumSpiders/InitialNumSpiders, na.rm=TRUE))
#vectors of differences F-T, ret then prop then fin num
fieldretpropfin3<-c((dat6[1,2]-dat6[2,2]),
                    (dat6[1,3]-dat6[2,3]),
                    (dat6[1,4]-dat6[2,4]))

tempretpropfin3=matrix(0, N, 3)
for (i in 1:N){
  temp=ddply(surv, "Treatment", transform, size=sample(Size, length(Size), replace=FALSE))
  tempdat=ddply(temp, "size", summarize, ret=mean(NumRetreats, na.rm=TRUE), prop=mean(PropDaysPrey), fin=mean(FinalNumSpiders/InitialNumSpiders, na.rm=TRUE))
  tempretpropfin3[i,]<-c((tempdat[1,2]-tempdat[2,2]),
                         (tempdat[1,3]-tempdat[2,3]),
                         (tempdat[1,4]-tempdat[2,4]))
  rm(temp, tempdat)
}

pmat6=matrix(0, N, 3)

for (i in 1:N){
  for (j in 1:3){
    pmat6[i,j]=if (abs(tempretpropfin3[i,j])<abs(fieldretpropfin3[j])) {0} else {1}
  }
}

psurv3=colSums(pmat6)/N
names(psurv3)=names(fieldretpropfin3)=c("retL-S", "propL-S", "finL-S")

#finally, for greenhouse trials.
#interaction between wind and treatment

ghsub$Wind=rep(0, nrow(ghsub))
for (i in 1:nrow(ghsub)){
  ghsub$Wind[i]= if (ghsub$Situation[i]=="Trial") {"nowind"} else {"wind"}
}

#with interaction
dat7=ddply(ghsub, c("Treatment", "Wind"), summarize, lat=mean(Latency, na.rm=TRUE), num=mean(Number, na.rm=TRUE))
#vectors of differences NF-WF, NT-WT, NF-NT, WF-WT; lat then num
ghlatnum<-c((dat7[1,3]-dat7[2,3]), (dat7[3,3]-dat7[4,3]), (dat7[1,3]-dat7[3,3]), (dat7[2,3]-dat7[4,3]), 
            (dat7[1,4]-dat7[2,4]), (dat7[3,4]-dat7[4,4]), (dat7[1,4]-dat7[3,4]), (dat7[2,4]-dat7[4,4]))

int3=c((ghlatnum[1]-ghlatnum[2]), (ghlatnum[3]-ghlatnum[4]), (ghlatnum[5]-ghlatnum[6]), (ghlatnum[7]-ghlatnum[8]))

#randomizing within trial week

tempghlatnum=matrix(0, N, 8)
for (i in 1:N){
  temp=ddply(ghsub, c("Batch"), transform, wind=sample(Wind, length(Wind), replace=FALSE), treatment=sample(Treatment, length(Treatment), replace=FALSE))
  tempdat=ddply(temp, c("treatment", "wind"), summarize, lat=mean(Latency, na.rm=TRUE), num=mean(Number, na.rm=TRUE))
  tempghlatnum[i,]<-c((tempdat[1,3]-tempdat[2,3]), (tempdat[3,3]-tempdat[4,3]), (tempdat[1,3]-tempdat[3,3]), (tempdat[2,3]-tempdat[4,3]), 
                    (tempdat[1,4]-tempdat[2,4]),(tempdat[3,4]-tempdat[4,4]), (tempdat[1,4]-tempdat[3,4]), (tempdat[2,4]-tempdat[4,4]))
  rm(temp, tempdat)
}

tempint3=cbind(tempghlatnum[,1]-tempghlatnum[,2], tempghlatnum[,3]-tempghlatnum[,4], tempghlatnum[,5]-tempghlatnum[,6], tempghlatnum[,7]-tempghlatnum[,8])

pmat7=matrix(0, N, 8)
pint3=matrix(0, N, 4)
for (i in 1:N){
  for (j in 1:8){
    pmat7[i,j]=if (abs(tempghlatnum[i,j])<abs(ghlatnum[j])) {0} else {1}
  }
  for (j in 1:4){
    pint3[i,j]=if (abs(tempint3[i,j])<abs(int3[j])) {0} else {1}
  }
}

pgh=colSums(pmat7)/N
pghint=colSums(pint3)/N
names(pgh)=names(ghlatnum)=c("latNF-WF", "latNT-WT", "latNF-NT", "latWF-WT", "numNF-WF", "numNT-WT", "numNF-NT", "numWF-WT")

#holding wind constant
dat8=ddply(ghsub, "Treatment", summarize,  lat=mean(Latency, na.rm=TRUE), num=mean(Number, na.rm=TRUE))
#F-T, lat-num
ghlatnum2<-c(dat8[1,2]-dat8[2,2], dat8[1,3]-dat8[2,3])

tempghlatnum2=matrix(0, N, 2)

for (i in 1:N){
  temp=ddply(ghsub, c("Batch", "Wind"), transform, treatment=sample(Treatment, length(Treatment), replace=FALSE))
  tempdat=ddply(temp, c("treatment"), summarize, lat=mean(Latency, na.rm=TRUE), num=mean(Number, na.rm=TRUE))
  tempghlatnum2[i,]<-c((tempdat[1,2]-tempdat[2,2]),
                     (tempdat[1,3]-tempdat[2,3]))
  rm(temp, tempdat)
}

pmat8=matrix(0, N, 2)

for (i in 1:N){
  for (j in 1:2){
    pmat8[i,j]=if (abs(tempghlatnum2[i,j])<abs(ghlatnum2[j])) {0} else {1}
  }
}

pgh2=colSums(pmat8)/N
names(pgh2)=names(ghlatnum2)=c("latF-T", "numF-T")

#holding treatment constant
dat9=ddply(ghsub, "Wind", summarize,  lat=mean(Latency, na.rm=TRUE), num=mean(Number, na.rm=TRUE))
#N-W, lat-num
ghlatnum3<-c(dat9[1,2]-dat9[2,2], dat9[1,3]-dat9[2,3])

tempghlatnum3=matrix(0, N, 2)

for (i in 1:N){
  temp=ddply(ghsub, c("Batch", "Treatment"), transform, wind=sample(Wind, length(Treatment), replace=FALSE))
  tempdat=ddply(temp, c("wind"), summarize, lat=mean(Latency, na.rm=TRUE), num=mean(Number, na.rm=TRUE))
  tempghlatnum3[i,]<-c((tempdat[1,2]-tempdat[2,2]),
                       (tempdat[1,3]-tempdat[2,3]))
  rm(temp, tempdat)
}

pmat9=matrix(0, N, 2)

for (i in 1:N){
  for (j in 1:2){
    pmat9[i,j]=if (abs(tempghlatnum3[i,j])<abs(ghlatnum3[j])) {0} else {1}
  }
}

pgh3=colSums(pmat9)/N
names(pgh3)=names(ghlatnum3)=c("latN-W", "numN-W")

