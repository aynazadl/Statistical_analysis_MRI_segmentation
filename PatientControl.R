rm(list=ls())
ControlPatient<-read.csv(file.choose(),header = T,sep =";" )
ls()
ControlPatient<-data.frame(ControlPatient)
attach(ControlPatient)
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
levels(ID)
levels(Part)
levels(Labels)
levels(Modality)
summary(ControlPatient)

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
install.packages("ggplot2")
install.packages("ggpubr")
library(ggpubr)
ggboxplot(ControlPatient.cuisseDP, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.cuisseDP, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="DPCuisse")+stat_compare_means(aes(group=Group))
qqnorm(ControlPatient.cuisseDP$mean)
qqline(ControlPatient.cuisseDP$mean)
hist(ControlPatient.cuisseDP$mean,freq = FALSE)

wilcox.test(mean~Group, data = ControlPatient.cuisseDP)
pairwise.wilcox.test(mean,Group, data = ControlPatient.cuisseDP,p.adjust.method="none")

ControlPatient.cuisseDP<-as.data.frame(ControlPatient.cuisseDP)
##################################
library(dplyr)
library(plyr)
library(MASS)
library(car)
library(carData)
library(magrittr)
library(tidyverse)
help("dplyr")
library(lattice)
head(ControlPatient)
library(ggplot2)
dd<-ddply(ControlPatient,.(Part,Modality,Group,Labels),function(x) mean(x))
dd<-ddply(ControlPatient,.(Part,Modality,Labels),function(x) print((shapiro.test(x$mean)$p.value)))
d<-ddply(ControlPatient,.(Part,Modality,Labels),function(x) print((summary(x$mean))))
d2<-join(d,dd,by=NULL,type= "full")
d2 <- d2[ c("Part","Modality","Labels","Mean","V1") ]
d2$V2<-0.05
attach(d2)
attach(ControlPatient)
glm.model1 = glm(mean ~Modality+Labels+Group+Part+Part:Modality:Labels:Group, data=ControlPatient, family=poisson)
anova(glm.model1, test="Chisq")
summary(glm.model1)
#ControlPatient<-data.matrix(ControlPatient)
#is.matrix(ControlPatient)
#friedman.test(ControlPatient)
#wilcox.test(mean~Group, data = ControlPatient.cuisseDP)
#is.matrix(ControlPatient.cuisseDP)
#ControlPatient.cuisseDP<-data.matrix(ControlPatient.cuisseDP)
#friedman.test(ControlPatient.cuisseDP) 
#form1=subset(ControlPatient.cuisseDP,select = c("mean","Group","Labels"))
#form1
#posthoc.friedman.nemenyi.test(form1)
#summary(posthoc.friedman.nemenyi.test(ControlPatient.cuisseDP))
#install.packages("PMCMR")
#library(PMCMR)
#ControlPatient.cuisseDP<-as.data.frame(ControlPatient.cuisseDP)
#help("ggboxplot")
#help("kruskal.test")
#(.packages())
#######################################

shapiro.test(ControlPatient.cuisseFF$mean)
library(ggpubr)
ggboxplot(ControlPatient.cuisseFF, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.cuisseFF, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="FFCuisse")+stat_compare_means()
qqnorm(ControlPatient.cuisseFF$mean)
qqline(ControlPatient.cuisseFF$mean)
hist(ControlPatient.cuisseFF$mean,freq = FALSE)
wilcox.test(mean~Group, data = ControlPatient.cuisseFF)


shapiro.test(ControlPatient.cuisseMTR$mean)
library(ggpubr)
ggboxplot(ControlPatient.cuisseMTR, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.cuisseMTR, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="MTRCuisse")+stat_compare_means()
qqnorm(ControlPatient.cuisseMTR$mean)
qqline(ControlPatient.cuisseMTR$mean)
hist(ControlPatient.cuisseMTR$mean,freq = FALSE)
wilcox.test(mean~Group, data = ControlPatient.cuisseMTR)


shapiro.test(ControlPatient.cuisseT2$mean)
library(ggpubr)
ggboxplot(ControlPatient.cuisseT2, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.cuisseT2,x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="T2Cuisse")+stat_compare_means()
help("ggboxplot")
qqnorm(ControlPatient.cuisseT2$mean)
qqline(ControlPatient.cuisseT2$mean)
hist(ControlPatient.cuisseT2$mean,freq = FALSE)
wilcox.test(mean~Group, data = ControlPatient.cuisseT2)


shapiro.test(ControlPatient.jambeDP$mean)
ggboxplot(ControlPatient.jambeDP, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.jambeDP, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="DPJambe")+stat_compare_means()
qqnorm(ControlPatient.jambeDP$mean)
qqline(ControlPatient.jambeDP$mean)
hist(ControlPatient.jambeDP$mean,freq = FALSE)
ftest<-var.test(mean~Group,data = ControlPatient.jambeDP )
ftest
res<-t.test(mean~Group,data = ControlPatient.jambeDP,var.equal=TRUE)
res
##############################
#fit<-aov(mean~Group+Labels,data = ControlPatient.jambeDP)
#fit
#TukeyHSD(fit)
#plot(fit,1)
#library(car)
#leveneTest(mean~Group*Labels,data = ControlPatient.jambeDP)
#plot(fit,2)
#residu<-residuals(object = fit)
#shapiro.test(residu)
###############################

shapiro.test(ControlPatient.jambeFF$mean)
library(ggpubr)
ggboxplot(ControlPatient.jambeFF, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.jambeFF, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="FFJambe")+stat_compare_means()
qqnorm(ControlPatient.jambeFF$mean)
qqline(ControlPatient.jambeFF$mean)
hist(ControlPatient.jambeFF$mean,freq = FALSE)
wilcox.test(mean~Group, data = ControlPatient.jambeFF)

shapiro.test(ControlPatient.jambeMTR$mean)
library(ggpubr)
ggboxplot(ControlPatient.jambeMTR, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.jambeMTR, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="MTRJambe")+stat_compare_means()
qqnorm(ControlPatient.jambeMTR$mean)
qqline(ControlPatient.jambeMTR$mean)
hist(ControlPatient.jambeMTR$mean,freq = FALSE)
wilcox.test(mean~Group, data = ControlPatient.jambeMTR)

shapiro.test(ControlPatient.jambeT2$mean)
library(ggpubr)
ggboxplot(ControlPatient.jambeT2, x = "Group", y = "mean",color = "Group", palette = c("#00AFBB", "#E7B800"),ylab = "mean", xlab = "Group")
ggboxplot(ControlPatient.jambeT2, x = "Labels", y = "mean",color = "Group",ylab = "mean", xlab = "Labels",las=2,main="T2Jambe")+stat_compare_means()
qqnorm(ControlPatient.jambeT2$mean)
qqline(ControlPatient.jambeT2$mean)
hist(ControlPatient.jambeT2$mean,freq = FALSE)
wilcox.test(mean~Group, data = ControlPatient.jambeT2)

#####################################################################################################

#friedman.test(mean~Group|Labels,data = ControlPatient.jambeT2 )

#kruskal.test(mean[Group=="patient"]~Labels[Group=="patient"], data = ControlPatient.jambeT2)
#pairwise.wilcox.test(mean[Group=="patient"],Labels[Group=="patient"], data = ControlPatient.jambeT2,p.adjust.method="none")


#install.packages("pgirmess")
#library(pgirmess)
#kruskalmc(mean~Group,data = ControlPatient.jambeT2)
