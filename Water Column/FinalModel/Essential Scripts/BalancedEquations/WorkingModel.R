

# Package Load #

source("/Users/michael/Desktop/PhD/Research Methods/Water Column Models/Water Column/Essentials/PackageList.R")

# Load Parameters

source("/Users/michael/Desktop/PhD/Research Methods/Water Column Models/Water Column/FinalModel/Essential Scripts/BalancedEquations/Parameters.R")


system("R CMD SHLIB DiffModelch.c")
dyn.load("DiffModelch.so")


parms<-c(z=z[1:length(z)], s=s[1:length(s)], r=r[1:length(r)], 
         m=m[1:length(m)], depth=NEMO,odechoice=1, sinkingchoice=1, 
         growthchoice=1, n=57)
         


fcontrol<-list(method="linear", rule=2, ties="ordered")


out<-as.data.frame(deSolve::lsoda(xstart, times, func="derivsc", parms, 
        dllname="DiffModelch", initfunc="initmod", jactype="bandint", 
        bandup=1, banddown=1, initforc = "forcc", fcontrol=fcontrol,
        forcings=dat))



#Trim off depth layers beanth bathymetry#
out<-out[,1:(tail(parms,n=1)+3)]





