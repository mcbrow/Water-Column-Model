
IrmingerBasin<-as.data.frame(IrmingerBasin)
IrmingerBasin<-IrmingerBasin[,-1]

# Make a list of matrices for the forcings. #


DrivingData<-function(file){
  
  file<-dplyr::arrange(file, Layer)
  
  

file$Vertical_diffusivity<-ifelse(file$Vertical_diffusivity>0.15, 
                                          0.15, file$Vertical_diffusivity)


    temp1<-lapply(1:21, function(x) file[,x])
    temp2<-list()
    for(i in 1:21){
    temp2[[i]]<-lapply(0:74, function(x) temp1[[i]][1:72+x*72])
    }
    names(temp2)<-names(file)
    temp2[[3]]<-NULL
    temp2[[3]]<-NULL
    temp2[[16]]<-NULL
    temp2[[16]]<-NULL
    temp2[[16]]<-NULL
    temp2[[16]]<-NULL
    return(temp2)
    
}



Forcings<-DrivingData(IrmingerBasin)














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





forc<-list(forcings)



