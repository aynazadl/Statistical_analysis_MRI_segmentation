rm(list=ls())
ControlPatient<-read.csv(file.choose(),header = T,sep =";" )
ls()
ControlPatient<-data.frame(ControlPatient)
attach(ControlPatient)
levels(ID)
levels(Part)
levels(Labels)
levels(Modality)
ID<-as.factor(ID)
class(ID)
Part<-as.factor(Part)
class(Part)
Labels<-as.factor(Labels)
class(Labels)
Modality<-as.factor(Modality)
class(Modality)
mean<-as.numeric(mean)
class(mean)
std<-as.numeric(std)
class(std)
Group<-as.factor(Group)
class(Group)
volume<-as.numeric(volume)
class(volume)
summary(ControlPatient)

#ControlPatient<-data.matrix(ControlPatient)

#lapply(ControlPatient$Part=1,)

#if (Part=="cuisse" & Modality=="DP"){ print()}
#help(package= d_ply)
#install.packages("dplyr")
#install.packages("plyr")
library(dplyr)
library(plyr)
library(MASS)
library(car)
library(carData)
library(magrittr)
library(tidyverse)
#d_ply()
#help("ddply")
help("dplyr")
library(lattice)
head(ControlPatient)
#attach(d)
library(ggplot2)

#dd<-data.frame(dd)
dd<-ddply(ControlPatient,.(Part,Modality,Group,Labels),function(x) print((shapiro.test(x$mean)$p.value)))

d<-ddply(ControlPatient,.(Part,Modality,Group,Labels),function(x) print((summary(x$mean))))
d2<-join(d,dd,by=NULL,type= "full")
d2 <- d2[ c("Part","Modality", "Group","Labels","Mean","V1") ]

#cp1<-ControlPatient %>% 
  group_by("Part")
#cp2<-filter(ControlPatient,Part, Modality)
d2$V2<-0.05
attach(d2)
d21<-as.list(d2)
rm(y)
#####################################################
for (i in 1:length(mean)){
  
  
}
  
  if (i<0.05){
 print(wilcox.test(mean[Labels]~Group,data = ControlPatient))
  } else {
 print(pairwise.t.test(Mean~Group+Labels,data = d2))
  }

######################################################
ControlPatientN<-read.csv(file.choose(),header = T,sep =";" )
ls()
ControlPatientN<-data.frame(ControlPatientN)
attach(ControlPatientN)
levels(ID)
levels(Part)
levels(Labels)
levels(Modality)
ID<-as.factor(ID)
class(ID)
Part<-as.factor(Part)
class(Part)
Labels<-as.factor(Labels)
class(Labels)
Modality<-as.factor(Modality)
class(Modality)
mean<-as.numeric(mean)
class(mean)
std<-as.numeric(std)
class(std)
Group<-as.factor(Group)
class(Group)
volume<-as.numeric(volume)
class(volume)
summary(ControlPatient)
rm(index0)
index=ControlPatientN$Labels=="Nerf"
ControlPatientN.Nerf=ControlPatientN[index,]
summary(ControlPatientN.Nerf)
ggboxplot(ControlPatientN.Nerf, x = "Modality", y = "mean",color = "Group",ylab = "mean", xlab = "Modality",las=2,main="Nerf",add = "jitter")+stat_compare_means(aes(group=Group),label = "p.format")

########################################################

index1=ControlPatient$Part=="cuisse" & ControlPatient$Modality=="DP"
ControlPatient.cuisseDP=ControlPatient[index1,]
summary(ControlPatient.cuisseDP)

index2=ControlPatient$Part=="cuisse" & ControlPatient$Modality=="FF"
ControlPatient.cuisseFF=ControlPatient[index2,]
summary(ControlPatient.cuisseFF)

index3=ControlPatient$Part=="cuisse" & ControlPatient$Modality=="MTR"
ControlPatient.cuisseMTR=ControlPatient[index3,]
summary(ControlPatient.cuisseMTR)

index4=ControlPatient$Part=="cuisse" & ControlPatient$Modality=="T2"
ControlPatient.cuisseT2=ControlPatient[index4,]
summary(ControlPatient.cuisseT2)

index5=ControlPatient$Part=="jambe" & ControlPatient$Modality=="DP"
ControlPatient.jambeDP=ControlPatient[index5,]
summary(ControlPatient.jambeDP)

index6=ControlPatient$Part=="jambe" & ControlPatient$Modality=="FF"
ControlPatient.jambeFF=ControlPatient[index6,]
summary(ControlPatient.jambeFF)

index7=ControlPatient$Part=="jambe" & ControlPatient$Modality=="MTR"
ControlPatient.jambeMTR=ControlPatient[index7,]
summary(ControlPatient.jambeMTR)

index8=ControlPatient$Part=="jambe" & ControlPatient$Modality=="T2"
ControlPatient.jambeT2=ControlPatient[index8,]
summary(ControlPatient.jambeT2)

shapiro.test(ControlPatient.cuisseDP$mean)
library(ggpubr)
ggboxplot(ControlPatient.cuisseDP, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.cuisseDP, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="DPCuisse",add = "jitter")+stat_compare_means(aes(group=Group))
qqnorm(ControlPatient.cuisseDP$mean)
qqline(ControlPatient.cuisseDP$mean)
hist(ControlPatient.cuisseDP$mean,freq = FALSE)

wilcox.test(mean~Group, data = ControlPatient.cuisseDP)
pairwise.wilcox.test(mean,Group, data = ControlPatient.cuisseDP,p.adjust.method="none")

ControlPatient.cuisseDP<-as.data.frame(ControlPatient.cuisseDP)
shapiro.test(ControlPatient.cuisseFF$mean)
library(ggpubr)
ggboxplot(ControlPatient.cuisseFF, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.cuisseFF, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="FFCuisse",add = "jitter")+stat_compare_means(aes(group=Group))
qqnorm(ControlPatient.cuisseFF$mean)
qqline(ControlPatient.cuisseFF$mean)
hist(ControlPatient.cuisseFF$mean,freq = FALSE)
wilcox.test(mean~Group, data = ControlPatient.cuisseFF)


shapiro.test(ControlPatient.cuisseMTR$mean)
library(ggpubr)
ggboxplot(ControlPatient.cuisseMTR, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.cuisseMTR, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="MTRCuisse",add = "jitter")+stat_compare_means(aes(group=Group))
qqnorm(ControlPatient.cuisseMTR$mean)
qqline(ControlPatient.cuisseMTR$mean)
hist(ControlPatient.cuisseMTR$mean,freq = FALSE)
wilcox.test(mean~Group, data = ControlPatient.cuisseMTR)


shapiro.test(ControlPatient.cuisseT2$mean)
library(ggpubr)
ggboxplot(ControlPatient.cuisseT2, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.cuisseT2,x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="T2Cuisse",add = "jitter")+stat_compare_means(aes(group=Group))
help("ggboxplot")
qqnorm(ControlPatient.cuisseT2$mean)
qqline(ControlPatient.cuisseT2$mean)
hist(ControlPatient.cuisseT2$mean,freq = FALSE)
wilcox.test(mean~Group, data = ControlPatient.cuisseT2)


shapiro.test(ControlPatient.jambeDP$mean)
ggboxplot(ControlPatient.jambeDP, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.jambeDP, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="DPJambe",add = "jitter")+stat_compare_means(aes(group=Group))
qqnorm(ControlPatient.jambeDP$mean)
qqline(ControlPatient.jambeDP$mean)
hist(ControlPatient.jambeDP$mean,freq = FALSE)
ftest<-var.test(mean~Group,data = ControlPatient.jambeDP )
ftest
res<-t.test(mean~Group,data = ControlPatient.jambeDP,var.equal=TRUE)
res


shapiro.test(ControlPatient.jambeFF$mean)
library(ggpubr)
ggboxplot(ControlPatient.jambeFF, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.jambeFF, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="FFJambe",add = "jitter")+stat_compare_means(aes(group=Group))
qqnorm(ControlPatient.jambeFF$mean)
qqline(ControlPatient.jambeFF$mean)
hist(ControlPatient.jambeFF$mean,freq = FALSE)
wilcox.test(mean~Group, data = ControlPatient.jambeFF)

shapiro.test(ControlPatient.jambeMTR$mean)
library(ggpubr)
ggboxplot(ControlPatient.jambeMTR, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.jambeMTR, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="MTRJambe",add = "jitter")+stat_compare_means(aes(group=Group))
qqnorm(ControlPatient.jambeMTR$mean)
qqline(ControlPatient.jambeMTR$mean)
hist(ControlPatient.jambeMTR$mean,freq = FALSE)
wilcox.test(mean~Group, data = ControlPatient.jambeMTR)

shapiro.test(ControlPatient.jambeT2$mean)
library(ggpubr)
ggboxplot(ControlPatient.jambeT2, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.jambeT2, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="T2Jambe",add = "jitter")+stat_compare_means(aes(group=Group))
qqnorm(ControlPatient.jambeT2$mean)
qqline(ControlPatient.jambeT2$mean)
hist(ControlPatient.jambeT2$mean,freq = FALSE)
wilcox.test(mean~Group, data = ControlPatient.jambeT2)
#rm(test)  
#ifelse(d2$V1<0.05,print(wilcox.test(Mean~Group,data = d2)),print(t.test(Mean~Group,data = d2)))

help("kruskal.test")
help("vars")
help("ifelse")
#as.data.frame(d)
#ControlPatient<- ControlPatient[,c('mean')]
#ControlPatient1<-ddply(ControlPatient,.(Part,Modality,Group,Labels),c("mean"))
#db<-ddply(ControlPatient,.(Part,Modality,Labels),function(x) print(shapiro.test(x$mean)))
#attr(d,"split_type")
#summary(dd)
#is.data.frame(dd)
#length(d)
#attach(dd)
#attach(d)
#length(d)
#summary(d)
#summary(dlply)
#d$cuisse.DP.Adductor
#d$jambe.T2.Nerf
#d$jambe.T2.Lateral_compartment        
#head(dd)
#help("attr")
#attr(d,"split_labels")
#attr(d,"names")
#d<-data.frame(d)
#dd<-data.frame(dd)
#join()
#as.data.frame(d)
#as.data.frame(dd)
#d2<-join(d,dd,by=NULL,type= "left")
