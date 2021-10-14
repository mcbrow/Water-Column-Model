library(raster)
library(tidyverse)
library(furrr)
library(tictoc)
library(ncdf4)
library(stringr)



get_ptrc_T_   <- function(filename, date) {
  
  print(stringr::str_glue("{filename} Extracting Dissolved Inorganic Nitrogen, Chlorophyll"))
  nc_raw <- ncdf4::nc_open(filename)                                # Open up a netcdf file to see it's raw contents (var names)
  nc_DIN <- ncdf4::ncvar_get(nc_raw, "DIN", start=c(1000,250,1,1), count=c(1,1,-1,-1))          # Extract an array of Dissolved inorganic nitrogen
  nc_CHD <- ncdf4::ncvar_get(nc_raw, "CHD", start=c(1000,250,1,1), count=c(1,1,-1,-1)) # Extract an array of CHD
  nc_CHN <- ncdf4::ncvar_get(nc_raw, "CHN", start=c(1000,250,1,1), count=c(1,1,-1,-1))
  nc_Chl<-nc_CHD+nc_CHN
  ncdf4::nc_close(nc_raw)                                                      # You must close an open netcdf file when finished to avoid data loss
  
  all <- data.frame(                                                                # Bind as columns
    DIN = nc_DIN,
    Chlorophyll = nc_Chl,
    Date = date) %>% 
    rownames_to_column(var="Depth")
  return(all)
}



files<-list.files( "I:/Science/MS/Shared/CAO/nemo/ALLARC", recursive=TRUE, 
                  full.names = TRUE) %>% 
  as.data.frame() %>% 
  rename(filename='.') %>% 
  mutate(date=str_sub(filename, end=-4, start=-11))

plan("multiprocess")

tic()
grid_T<-filter(files, str_detect(filename, 'ptrc_T_')) %>% 
  future_pmap(get_ptrc_T_, .progress=TRUE) %>% 
  data.table::rbindlist()
toc()