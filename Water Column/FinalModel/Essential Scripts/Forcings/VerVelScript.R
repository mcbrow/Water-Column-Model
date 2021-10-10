


VerVel<- function(filename, date, x, y){
  
  
  
  nc_raw <- ncdf4::nc_open(filename)
  nc_dif <- ncdf4::ncvar_get(nc_raw, "votkeavt", start=c(x,y,1,1), count=c(1,1,-1,-1))
  ncdf4::nc_close(nc_raw)
  
  
  data <- data.frame(
    VerticalVelocity= nc_dif,
    Date = date) %>% 
    rownames_to_column(var="Depth")
  return(data)
  
}



  
  


  