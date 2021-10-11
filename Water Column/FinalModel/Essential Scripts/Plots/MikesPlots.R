#------------------
#------------------

#plot and individual profiles at time=ttt (including the number of particles accumulated in the export layer 
ttt<-720
plot(as.numeric(out[ttt,2:(tail(parms,n=1)+3)]),NEMO[1:length(NEMO[1:(tail(parms,n=1)+2)])],type="l", xlab="Particles", 
     ylab="Depth", ylim=rev(range(NEMO[1:length(NEMO[1:(tail(parms,n=1)+2)])])) )


#------------------

#Animate the vertical profile of particle concentrations over time
ymax<-0
for(zzz in 1:tail(parms,n=1)+2){
  ymax<-max(ymax,out[,zzz+1]/z[zzz])
}
ymax<-1.1*ymax


for(ttt in 1:(length(times)-1)){
  #plot(as.numeric(out[ttt,2:(length(z)+2])),seq(1,110,by=10),xlim=c(0,100), ylim=rev(range(c(100,0))),type="l")
  plot(as.numeric(out[ttt,2:(tail(parms,n=1)+2)]/z),cz[1:(tail(parms,n=1)+1)],ylim=rev(range(c(0,max(cz[1:(tail(parms,n=1)+1)])))),type="l", xlim=c(0,2*max(z[tail(parms,n=1)+1])),
       ylab="Sea Depth (m)", xlab="Particle Concentration", main=paste0("Particle Concentration in Water Column at timestep ", ttt))
}


#------------------

#Plot the time series of cumulative number of particles which have emerged from the base of the water column
plot(out[,(tail(parms,n=1)+3)],type="l", ylab="No. of Particles", xlab="Time",
     main="seabed abundance") # seabed abundance1


#------------------

#Plot the time series of particles flux emerging from the base of the water column
temp<-rep(NA,length(times))
temp[1:(length(times)-1)]<-out[2:length(times),tail(parms,n=1)+3]
out[,(tail(parms,n=1)+4)]<-temp-out[,(tail(parms,n=1)+3)]
plot(out[,(tail(parms,n=1)+4)],type="l", ylab="Flux of particles", xlab="Time", main="Particle Flux from base of water column") # seabed flux


#Plot the time series of number of particles remaining in the water column
out[,tail(parms,n=1)+5]<-rowSums(out[,2:(tail(parms,n=1)+2)]) # total depth integrated population in the water column
plot(out[,tail(parms,n=1)+5],type="l", ylab="No. of Particles", xlab="Time", main="Particles in water column") # depth integrated population


#------------------

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
