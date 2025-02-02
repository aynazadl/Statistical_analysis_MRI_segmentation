
#power.t.test for data of thigh-calf 
rm(list=ls())

rm(p1)
help("seq")


library(ggpubr)
library(ggplot2)
library(magrittr)
dff=data.frame(nsizeff,deltaff,level)
dff
dcsa=data.frame(nsizecsa,deltacsa,level)
dcsa
deltaff<-as.numeric(deltaff)
class(deltaff)
w1=ggline(data= dff,x = "deltaff", y = "nsizeff", color = "level",
         palette = c("#00AFBB", "#E7B800"))
w2=ggline(data = dcsa,x = "deltacsa", y = "nsizecsa", color = "level",
          add = c("mean_se"),
          palette = c("#00AFBB", "#E7B800"))


##############################################################
delta=seq(0.1,10,by=0.1)
delta
level=c("thigh","calf")
pwr.n=c()
delta.i=c()
for (i in 1:100){
  pwr=power.t.test(power = 0.8,delta = delta[i],sd=1.5,sig.level = 0.05 )
  df=data.frame(pwr$n,delta[i])
  print(df)
  
}