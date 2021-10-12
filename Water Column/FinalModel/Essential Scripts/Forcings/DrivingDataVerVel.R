times<-seq(1,365,1)
nyears<-2



IrmingerData1<-arrange(IrmingerData, Layer)
IrmingerData1<-IrmingerData
IrmingerData1$Vertical_diffusivity<-ifelse(IrmingerData1$Vertical_diffusivity>0.15, 
                                           0.15, IrmingerData1$Vertical_diffusivity)

Vertical_diffusivity<-as.data.frame(IrmingerData1$Vertical_diffusivity)

VerDiff<-list()

for(i in 0:74){
  
  
  VerDiff[[i+1]]<-as.matrix(data.frame(times=seq(1,72,1), VerDiff=Vertical_diffusivity[i+(1:72),]))
    
}

IrmingerData1$Temperature
Temperature<-as.data.frame(IrmingerData1$Temperature)


Temp<-list()

for(i in 0:74){
  
  
  Temp[[i+1]]<-as.matrix(data.frame(times=seq(1,72,1), VerDiff=Temperature[i+(1:72),]))
  
}









replicate_data<-function(file){ 
  obs<-nrow(file) 
drtimes<-data.frame(file[,1],file[,2]) 
if(nyears>1){
  for(j in 1:(nyears-1)){ 
    drtimes<-rbind(drtimes,(data.frame(file[,1],file[,2])))
  }} 
if(nyears>1){
    for(j in (obs+1):(obs*nyears)){ 
      drtimes[j,1]<-(drtimes[(j-obs),1]+365)
    }}
return(drtimes)
}




replicate_data()








forc<-list(forcings)



