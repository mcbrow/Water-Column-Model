library(raster)
library(tidyverse)
library(furrr)
library(tictoc)
library(ncdf4)



get_grid_U   <- function(filename, date) {
  
  print(stringr::str_glue("{filename} Extracting Zonal Currents"))
  nc_raw <- ncdf4::nc_open(filename)                                # Open up a netcdf file to see it's raw contents (var names)
  nc_zonal <- ncdf4::ncvar_get(nc_raw, "vozocrtx", start=c(n,m,1,1), count=c(1,1,-1,-1))          # Extract an array of Zonal Current
  ncdf4::nc_close(nc_raw)                                                      # You must close an open netcdf file when finished to avoid data loss
  
  all <- data.frame(                                                                # Bind as columns
    zonal=nc_zonal,
    Date = date) %>% 
    tibble::rownames_to_column(var="Depth")
  return(all)
}



files<-list.files("/Volumes/webdrive.strath.ac.uk/idrive/Science/MS/Shared/CAO/nemo/ALLARC", recursive=TRUE, 
                  full.names = TRUE) %>% 
  as.data.frame() %>% 
  rename(filename='.') %>% 
  mutate(date=str_sub(filename, end=-4, start=-11))

plan("multiprocess")
tic()
grid_U<-filter(files, str_detect(filename, 'grid_U_')) %>% 
  future_pmap(get_grid_U, .progress=TRUE) %>% 
  data.table::rbindlist()
toc()





