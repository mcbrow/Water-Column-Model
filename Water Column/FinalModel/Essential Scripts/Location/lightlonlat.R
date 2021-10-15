GetCoor<-function(path){
  
  
  print(stringr::str_glue("{path}, Extracting Coordinates"))
  nc<-nc_open(path)
  nc_got<-ncvar_get(nc, "surface", start=c(1,1,1), count(1,1,-1))
  
  
  
  
  return(nc_got)
}

GetCoor("/Users/michael/NEMOMEDUSA/light/SWF_rcp85_y2012.nc")

Long<-as.character(Long)
