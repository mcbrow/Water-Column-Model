



get_Light  <- function(filename, Lon, Lat) {
  
  source("/Users/michael/Desktop/PhD/Research Methods/Water Column Models/Water Column/FinalModel/Essential Scripts/Location/LightConverter.R")
  if(Lon<0){
    Lon=360+Lon
  }
  
  temp<-FindLatLon(Lat, Lon)
  
  print(stringr::str_glue("{filename} Extracting Surface Light"))
  
  
  
  nc_raw <- ncdf4::nc_open(filename)                                # Check Var Names
  nc_SWF <-ncdf4::ncvar_get(nc_raw, "SWF", start=c(temp$Long,temp$Lat,1), count=c(1,1,-1))          # Extract Light
  
  ncdf4::nc_close(nc_raw)                                                      # Close avoid data loss
  
  all<-data.frame(nc_SWF)
  
#  ifelse(write==TRUE || T,
 # write.csv(all, "light.csv"),
  #          NULL)
  
  
  return(nc_SWF)
}




lightdata<-get_Light("/Users/michael/NEMOMEDUSA/light/SWF_rcp85_y2017.nc", -33, 61)









