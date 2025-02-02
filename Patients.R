rm(list=ls())
Patients<-read.csv(file.choose(),header = T,sep =";" )
ls()
Patients<-data.frame(Patients)
attach(Patients)
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
volume<-as.numeric(volume)
class(volume)
levels(ID)
levels(Part)
levels(Labels)
levels(Modality)
summary(Patients)
index=Patients$Part=="cuisse"
Patients.cuisse=Patients[index,]
summary(Patients.cuisse)
index1=Patients$Part=="jambe"
Patients.jambe=Patients[index1,]
summary(Patients.jambe)

help("pairwise.wilcox.test")

library("ggpubr")
ggboxplot(Patients.cuisse, x = "Labels", y = "mean", color = "Modality")
ggboxplot(Patients.jambe, x = "Labels", y = "mean", color = "Modality")

boxplot(mean[Modality=="DP"]~Labels[Modality=="DP"],las=2,data = Patients.cuisse)
qqnorm(Patients.cuisse$mean[Modality=="DP"])
qqline(Patients.cuisse$mean[Modality=="DP"])
shapiro.test(Patients.cuisse$mean[Modality=="DP"]) #nonparam
wilcox.test(mean[Modality=="DP"]~Labels[Modality=="DP"], data = Patients.cuisse)


boxplot(mean[Modality=="FF"]~Labels[Modality=="FF"],las=2,data = Patients.cuisse)
qqnorm(Patients.cuisse$mean[Modality=="FF"])
qqline(Patients.cuisse$mean[Modality=="FF"])
shapiro.test(Patients.cuisse$mean[Modality=="FF"]) #nonparam
wilcox.test(mean[Modality=="FF"]~Labels[Modality=="FF"], data = Patients.cuisse)

boxplot(mean[Modality=="MTR"]~Labels[Modality=="MTR"],las=2,data = Patients.cuisse)
qqnorm(Patients.cuisse$mean[Modality=="MTR"])
qqline(Patients.cuisse$mean[Modality=="MTR"])
shapiro.test(Patients.cuisse$mean[Modality=="MTR"]) #nonparam
wilcox.test(mean[Modality=="MTR"]~Labels[Modality=="MTR"], data = Patients.cuisse)

boxplot(mean[Modality=="T2"]~Labels[Modality=="T2"],las=2,data = Patients.cuisse)
qqnorm(Patients.cuisse$mean[Modality=="T2"])
qqline(Patients.cuisse$mean[Modality=="T2"])
shapiro.test(Patients.cuisse$mean[Modality=="T2"]) #nonparam
wilcox.test(mean[Modality=="T2"]~Labels[Modality=="T2"], data = Patients.cuisse)

boxplot(mean[Modality=="DP"]~Labels[Modality=="DP"],las=2,data = Patients.jambe)
qqnorm(Patients.jambe$mean[Modality=="DP"])
qqline(Patients.jambe$mean[Modality=="DP"])
shapiro.test(Patients.jambe$mean[Modality=="DP"]) #nonparam
wilcox.test(mean[Modality=="DP"]~Labels[Modality=="DP"], data = Patients.jambe)

boxplot(mean[Modality=="FF"]~Labels[Modality=="FF"],las=2,data = Patients.jambe)
qqnorm(Patients.jambe$mean[Modality=="FF"])
qqline(Patients.jambe$mean[Modality=="FF"])
shapiro.test(Patients.jambe$mean[Modality=="FF"])#nonparam
wilcox.test(mean[Modality=="FF"]~Labels[Modality=="FF"], data = Patients.jambe)

boxplot(mean[Modality=="MTR"]~Labels[Modality=="MTR"],las=2,data = Patients.jambe)
qqnorm(Patients.jambe$mean[Modality=="MTR"])
qqline(Patients.jambe$mean[Modality=="MTR"])
shapiro.test(Patients.jambe$mean[Modality=="MTR"])#nonparam
wilcox.test(mean[Modality=="MTR"]~Labels[Modality=="MTR"], data = Patients.jambe)

boxplot(mean[Modality=="T2"]~Labels[Modality=="T2"],las=2,data = Patients.jambe)
qqnorm(Patients.jambe$mean[Modality=="T2"])
qqline(Patients.jambe$mean[Modality=="T2"])
shapiro.test(Patients.jambe$mean[Modality=="T2"])#nonparam
wilcox.test(mean[Modality=="T2"]~Labels[Modality=="T2"], data = Patients.jambe)

