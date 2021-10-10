library(tidyverse)
library(tibble)
library(ncdf4)
library(stringr)



get_ptrc_T_ <- function(filename, date, n, m) {
  
  print(stringr::str_glue("{filename} Extracting Dissolved Inorganic Nitrogen, Chlorophyll, Mesozooplankton, Microzooplankton and Detritus"))
  nc_raw <- ncdf4::nc_open(filename)                                # Open up a netcdf file to see it's raw contents (var names)
  nc_DIN <- ncdf4::ncvar_get(nc_raw, "DIN", start=c(n,m,1,1), count=c(1,1,-1,-1))          # Extract an array of Dissolved inorganic nitrogen
  nc_CHD <- ncdf4::ncvar_get(nc_raw, "CHD", start=c(n,m,1,1), count=c(1,1,-1,-1)) # Extract an array of CHD
  nc_CHN <- ncdf4::ncvar_get(nc_raw, "CHN", start=c(n,m,1,1), count=c(1,1,-1,-1)) # Extract an array of CHN
  nc_DET <- ncdf4::ncvar_get(nc_raw, "DET", start=c(n,m,1,1), count=c(1,1,-1,-1))      #Extract an array of Detritus
  nc_ZME <- ncdf4::ncvar_get(nc_raw, "ZME", start=c(n,m,1,1), count=c(1,1,-1,-1)) # Extract an array of mesozooplankton
  nc_ZMI <- ncdf4::ncvar_get(nc_raw, "ZMI", start=c(n,m,1,1), count=c(1,1,-1,-1)) # Extract an array of microzooplankton
  nc_Chl<-nc_CHD+nc_CHN
  ncdf4::nc_close(nc_raw)                                                      # You must close an open netcdf file when finished to avoid data loss
  
  all <- data.frame(                                                                # Bind as columns
    DIN = nc_DIN,
    Chlorophyll = nc_Chl,
    DET=nc_DET,
    ZME=nc_ZME,
    ZMI=nc_ZMI,
    Date = date) %>% 
    tibble::rownames_to_column(var="Depth")
  return(all)
}




