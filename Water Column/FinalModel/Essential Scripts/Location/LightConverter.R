library(sf)
library(nngeo)
library(data.table)

# Nearest Neighbour converter to find NETCDF 2D index to relate to geographic coordinates #

FindLatLon<-function(lat, lon){
 
  source("/Users/michael/Desktop/PhD/Research Methods/Water Column Models/Water Column/FinalModel/Essential Scripts/Location/CoordinateforLight.R")
  
  DT<-data.table::data.table(lat, lon)  
  DT_sf<-sf::st_as_sf(Coordinates, coords=c("Latitude", "Longitude"))
  DT_sf1<-sf::st_as_sf(DT, coords=c("lat", "lon"), agr="constant")
  join<-sf::st_join(DT_sf1, DT_sf, nngeo::st_nn, k=1)
  
  
  return(join)
  
  
}







