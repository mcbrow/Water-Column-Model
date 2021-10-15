
nc_raw<-ncdf4::nc_open("/Users/michael/NEMOMEDUSA/light/SWF_history_y1981.nc")

Long1<-nc_raw$dim$longitude$vals
Lat<-nc_raw$dim$latitude$vals
Longholder<-list()
  for(i in 1:nrow(nc_raw$dim$longitude$vals)){
    Longholder[[i]]<-rep(nc_raw$dim$longitude$vals[i], nrow(Lat))
  }
  Long<-c(unlist(Longholder))

Index1<-rep(seq(1,nrow(Long1)), nrow(Lat))
Indextemp<-list()



temp2<-list()
temp3<-c(unlist(temp2))

for(i in 1:nrow(Lat)){
  temp2[[i]]<-rep(Lat[i], nrow(Long1)) 
  
}

for(j in 1:nrow(Long1)){
  Indextemp[[j]]<-rep(j, nrow(Lat))
}

Index2<-unlist(Indextemp)


Coordinates<-data.frame(Lat=Index1,
                        Long=Index2,
                        Latitude=Lat,
                        Longitude=Long
                        
                
                        
)
nc_close(nc_raw)

