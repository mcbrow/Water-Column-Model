install.packages("sf", type ='source', repo ='cran.rstudio.com')
install.packages("raster")
install.packages("rgdal", repo = 'https://mac.R-project.org')
remotes::install_github("r-spatial/sf", configure.args = "--with-proj-lib=/usr/local/lib/")


install.packages("nngeo")
library(tidyverse)
library(ncdf4)
library(tidyr)
library(raster)
library(data.table)
library(units)
library(sf)
library(nngeo)
library(reshape2)
library(devtools)
library(rgdal)


#get lon lat coordinates by giving X Y points and searching nc file
lonlatcoor<-function(X, Y, path){
  
  nc<-nc_open(path)
  nc.lon<-ncvar_get(nc, "nav_lon") 
  nc.lat<-ncvar_get(nc, "nav_lat") 
  
  lon1<-setNames(reshape2::melt(nc.lon), c("X", "Y", "lon"))
  lat1<-setNames(reshape2::melt(nc.lat), c("X", "Y", "lat"))
  loc<-merge(lon1, lat1, by=c("X", "Y"), sort = TRUE, all =TRUE)
  DT<-data.table(X, Y)
  DT_sf<-st_as_sf(loc, coords=c("X", "Y"))
  DT_sf1<-st_as_sf(DT, coords=c("X", "Y"), agr="constant")
  a<-st_join(DT_sf1, DT_sf, st_nn, k=1)
  return(a)
}

#get x-y points by giving lon and lat coordinates and searching nc file


xycoor<-function(lon, lat, path){
  
  nc<-nc_open(path)
  nc.lon<-ncvar_get(nc, "nav_lon") 
  nc.lat<-ncvar_get(nc, "nav_lat") 
  
  
  lon1<-setNames(melt(nc.lon), c("X", "Y", "lon"))
  lat1<-setNames(melt(nc.lat), c("X", "Y", "lat"))
  loc<-merge(lon1, lat1, by=c("X", "Y"), sort = TRUE, all =TRUE)
  DT<-data.table(lon ,lat)
  DT_sf<-st_as_sf(loc, coords=c("lon", "lat"))
  DT_sf1<-st_as_sf(DT, coords=c("lon", "lat"), agr="constant")
  a<-st_join(DT_sf1, DT_sf, st_nn, k=1)
  nc_close(nc)
  return(a)
}

xycoor(-33, 61, "/Users/michael/NEMOMEDUSA/ALLARC/1985/grid_T_19850130.nc")



