library(raster)
library(tidyverse)
library(furrr)
library(tictoc)
library(ncdf4)



get_grid_V_   <- function(filename, date) {
  
  print(stringr::str_glue("{filename} Extracting Meridonal Currents"))
  nc_raw <- ncdf4::nc_open(filename)                                # Open up a netcdf file to see it's raw contents (var names)
  nc_merid <- ncdf4::ncvar_get(nc_raw, "vomecrty", start=c(n,m,1,1), count=c(1,1,-1,-1))          # Extract an array of meridinal currents
  ncdf4::nc_close(nc_raw)                                                      # You must close an open netcdf file when finished to avoid data loss
  
  all <- data.frame(                                                                # Bind as columns
    merid = nc_merid,
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



tic()
grid_T<-filter(files, str_detect(filename, 'grid_V_')) %>% 
  future_pmap(get_grid_V_, .progress=TRUE) %>% 
  data.table::rbindlist()
toc()