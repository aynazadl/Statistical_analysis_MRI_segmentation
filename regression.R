
#regression analysing between MTR and FF an plotting it,control-patient separeted

patient<-read.csv(file.choose(),header = T,sep =";" )
control<-read.csv(file.choose(),header = T,sep ="," )


Modality1<-c("DP","MTR","FF","T2")
Modality1[1]

levels(Labels)
Labels1=c("Vastus_intermedius","Vastus_medialis","Vastus_lateralis","Redctus_femoris","Adductor","Semi_membranous","Semi_tendinous","Biceps_femoris","Soleus",
          "Sartorius","Gracilis", "Gastrocnemius_lateralis","Gastrocnemius_medialis", "Anterior_compartment","Deep_posterior_compartment","Lateral_compartment")
Labels1[2]
str(patient)
is.numeric(patient$mean)
#vectormodality=c()
#vectorcompartment=c()
#vectorpvalue=c()
for(j in 1:16){
    y=Labels1[j]
    A=data.frame(subset(patient,patient$Labels==y))
    print(all(is.na(A$mean[A$Modality=="MTR"])))
    p=lm(A$mean[A$Modality=="MTR"]~A$mean[A$Modality=="FF"],data = A)
    print(y)
    print(p)
    print(cor(A$mean[A$Modality=="MTR"],A$mean[A$Modality=="FF"]))
    print(paste("--------------------------------------------"))
    #vectormodality=c(vectormodality,Modality1[i])
    #vectorcompartment=c(vectorcompartment,Labels1[j])
    #vectorpvalue=c(vectorpvalue,round(p$p.value,digits = 4))
    

}

for(j in 1:16){
  y=Labels1[j]
  A=data.frame(subset(patient,patient$Labels==y))
  mypath <- file.path("C:","Users","Aynaz","Documents","regression",paste("ggPlot_",Labels1[j],".png", sep = ""))
  
  png(file=mypath)
  mytitle = paste("gglinePlot_","_",Labels1[j],sep = "")
  p=lm(A$mean[A$Modality=="MTR"]~A$mean[A$Modality=="FF"],data = A)
  v=with(A,plot(A$mean[A$Modality=="MTR"]~A$mean[A$Modality=="FF"],main ="scatter plot entre FF et MTR"))
  w=abline(p)
  print(y)
  print(p)
  print(v)
  print(w)
  dev.off()
  #vectormodality=c(vectormodality,Modality1[i])
  #vectorcompartment=c(vectorcompartment,Labels1[j])
  #vectorpvalue=c(vectorpvalue,round(p$p.value,digits = 4))
}
#here we know that between the "vastus-medialis", "rectus-femoris","sartorius" of patients MTR-FF we have no dependency 

for(j in 1:16){
  y=Labels1[j]
  A=data.frame(subset(control,control$Labels==y))
  print(all(is.na(A$mean[A$Modality=="MTR"])))
  p=lm(A$mean[A$Modality=="MTR"]~A$mean[A$Modality=="FF"],data = A)
  print(y)
  print(p)
  print(cor(A$mean[A$Modality=="MTR"],A$mean[A$Modality=="FF"]))
  print(paste("--------------------------------------------"))
  #vectormodality=c(vectormodality,Modality1[i])
  #vectorcompartment=c(vectorcompartment,Labels1[j])
  #vectorpvalue=c(vectorpvalue,round(p$p.value,digits = 4))
  
  
}

for(j in 1:16){
  y=Labels1[j]
  A=data.frame(subset(control,control$Labels==y))
  mypath <- file.path("C:","Users","Aynaz","Documents","regression1",paste("ggPlot_",Labels1[j],".png", sep = ""))
  
  png(file=mypath)
  mytitle = paste("gglinePlot_","_",Labels1[j],sep = "")
  p=lm(A$mean[A$Modality=="MTR"]~A$mean[A$Modality=="FF"],data = A)
  v=with(A,plot(A$mean[A$Modality=="MTR"]~A$mean[A$Modality=="FF"],main ="scatter plot entre FF et MTR"))
  w=abline(p)
  print(y)
  print(p)
  print(v)
  print(w)
  dev.off()
  #vectormodality=c(vectormodality,Modality1[i])
  #vectorcompartment=c(vectorcompartment,Labels1[j])
  #vectorpvalue=c(vectorpvalue,round(p$p.value,digits = 4))
}
