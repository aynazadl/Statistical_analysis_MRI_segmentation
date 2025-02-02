
#separeting the volume of FF and manipulating with mean FF and then plotting it

attach(file1)
file1<-read.csv(file.choose(),header = T,sep ="," )
FF=subset(file1,Modality=="FF")
volR=data.frame(FF$volume*(1-FF$mean))
dFF=data.frame(FF$ID,FF$Group,FF$Part,FF$Modality,FF$Labels,FF$Slice,FF$mean,FF$std,FF$volume,volR)
ModalityFF=FF$Modality
SliceFF=FF$Slice
LabelsFF=FF$Labels
GroupFF=FF$Group

library(ggpubr)
library(ggplot2)
library(magrittr)

Labels1=c("Vastus_intermedius","Vastus_medialis","Vastus_lateralis","Rectus_femoris","Adductor","Semi_membranous","Semi_tendinous","Biceps_femoris","Soleus",
          "Sartorius","Gracilis", "Gastrocnemius_lateralis","Gastrocnemius_medialis", "Anterior_compartment","Deep_posterior_compartment","Lateral_compartment")
Labels1[2]

for(j in 1:16){
    y=Labels1[j]
    B=data.frame(subset(dFF,dFF$FF.Labels==y))
    
    mypath <- file.path("C:","Users","Aynaz","Documents","FF1",paste("gglinePlot_",Labels1[j],".png", sep = ""))
    
    png(file=mypath)
    mytitle = paste("gglinePlot_",Labels1[j],sep = "")
    w=ggline(data = B, x = "FF.Slice", y = "FF.volume....1...FF.mean.", color = "FF.Group",
             add = c("mean_se"),
             palette = c("#00AFBB", "#E7B800"), main = mytitle)+scale_x_continuous("Slice", limits = c(1, 36), breaks = seq(1, 36, 5))
    
    
    print(j)
    print(y)
    print(w)
    dev.off()
  
}