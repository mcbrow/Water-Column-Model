times<-seq(1,365,1)
nyears<-2


vec<-2:76
fun<-function(x){
  data.frame(times=seq(1,72,1), 
             "VerticalVelocity"=data2[,x])
}

dat<-lapply(vec, fun)

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



