library(raster)
library(tidyverse)
library(furrr)
library(tictoc)
library(ncdf4)
library(nemomedusR)



get_grid_W_   <- function(filename, date) {
  
  print(stringr::str_glue("{filename} Extracting Vertical Velocity, Vertical Eddy Diffusivity"))
  nc_raw <- ncdf4::nc_open(filename)                                # Open up a netcdf file to see it's raw contents (var names)
  nc_vel <- ncdf4::ncvar_get(nc_raw, "vovercrtz", start=c(1021,173,1,1), count=c(1,1,-1,-1))          # Extract an array of velocities
  nc_dif <- ncdf4::ncvar_get(nc_raw, "votkeavt", start=c(1021,173,1,1), count=c(1,1,-1,-1)) # Extract an array of diffusivity
  ncdf4::nc_close(nc_raw)                                                      # You must close an open netcdf file when finished to avoid data loss
  
  all <- data.frame(                                                                # Bind as columns
    vel = nc_vel,
    Vertical_Diffusivity = nc_dif,
    Date = date) %>% 
    rownames_to_column(var="Depth")
  return(all)
}

files<-list.files("/Users/michael/NEMOMEDUSA/ALLARC", recursive=TRUE, 
                  full.names = TRUE) %>% 
  as.data.frame() %>% 
  rename(filename='.') %>% 
  mutate(date=str_sub(filename, end=-4, start=-11))

future::plan("multiprocess")

tic()
grid_T<-filter(files, str_detect(filename, 'grid_W_')) %>% 
  future_map(get_grid_W_, .progress=TRUE) %>% 
toc()


setwd("/Users/michael/Desktop/PhD/Research/Water Column Model/DATA/IrmingerBasin")

