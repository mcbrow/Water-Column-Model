get_Light  <- function(filename, date) {
  
  print(stringr::str_glue("{filename} Extracting Surface Light"))
  nc_raw <- ncdf4::nc_open(filename)                                # Open up a netcdf file to see it's raw contents (var names)
  nc_Surf <- ncdf4::ncvar_get(nc_raw, "surface", start=c(1021,174,1,1), count=c(1,1,-1,-1))          # Extract an array of Dissolved inorganic nitrogen
  
  ncdf4::nc_close(nc_raw)                                                      # You must close an open netcdf file when finished to avoid data loss
  
  all <- data.frame(                                                                # Bind as columns
    Surface = nc_Surf
    ) 
  return(all)
}

get_Light("/Users/michaelbrown/Desktop/SWF_history_y1980.nc")

