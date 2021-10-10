library(raster)
library(tidyverse)
library(furrr)
library(tictoc)
library(ncdf4)




get_sea   <- function(filename, date) {
  
  print(stringr::str_glue("{filename} Extracting Salinity, Temperature"))
  nc_raw <- ncdf4::nc_open(filename)                                # Open up a netcdf file to see it's raw contents (var names)
  nc_saline <- ncdf4::ncvar_get(nc_raw, "vosaline", start=c(n,m,1,1), count=c(1,1,-1,-1))          # Extract an array of salinities
  nc_temp <- ncdf4::ncvar_get(nc_raw, "votemper", start=c(n,m,1,1), count=c(1,1,-1,-1))            # Extract an array of temperatures
  ncdf4::nc_close(nc_raw)                                                      # You must close an open netcdf file when finished to avoid data loss
  
  all <- data.frame(                                                                # Bind as columns
    Salinity = nc_saline,
    Temperature = nc_temp,
    Date = date) %>% 
    rownames_to_column(var="Depth")
  return(all)
}

plan("multiprocess")

files<-list.files("I:/Science/MS/Shared/CAO/nemo/ALLARC", recursive=TRUE, 
                  full.names = TRUE) %>% 
  as.data.frame() %>% 
  rename(filename='.') %>% 
  mutate(date=str_sub(filename, end=-4, start=-11))

head(files)

tic()
grid_T<-filter(files, str_detect(filename, 'grid_T_')) %>% 
  future_pmap(get_sea, .progress=TRUE) %>% 
  data.table::rbindlist()
toc()

