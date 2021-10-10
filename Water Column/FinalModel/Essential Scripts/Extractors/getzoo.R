library(raster)
library(tidyverse)
library(furrr)
library(tictoc)
library(ncdf4)



get_ptrc_T_mod   <- function(filename, date) {
  
  print(stringr::str_glue("{filename} Extracting Dissolved Inorganic Nitrogen, Chlorophyll, Mesozooplankton, Microzooplankton and Detritus"))
  nc_raw <- ncdf4::nc_open(filename)                                # Open up a netcdf file to see it's raw contents (var names)
  nc_DET <- ncdf4::ncvar_get(nc_raw, "DET", start=c(n,m,1,1), count=c(1,1,-1,-1))          # Extract an array of Dissolved inorganic nitrogen
  nc_ZME <- ncdf4::ncvar_get(nc_raw, "ZME", start=c(n,m,1,1), count=c(1,1,-1,-1)) # Extract an array of CHD
  nc_ZMI <- ncdf4::ncvar_get(nc_raw, "ZMI", start=c(n,m,1,1), count=c(1,1,-1,-1))
  ncdf4::nc_close(nc_raw)                                                      # You must close an open netcdf file when finished to avoid data loss
  
  all <- data.frame(                                                                # Bind as columns
    DET=nc_DET,
    ZME=nc_ZME,
    ZMI=nc_ZMI,
    Date = date) %>% 
    rownames_to_column(var="Depth")
  return(all)
}






