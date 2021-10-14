
Irrad<-read.csv("/Users/michael/Desktop/PhD/Research Methods/Water Column Models/Water Column/Scrap/irradiance_driving_data.csv")


library(ncdf4)

nc<-nc_open("/Users/michael/NEMOMEDUSA/light/SWF_rcp85_y2017.nc")
nc1<-nc_open("/Users/michael/NEMOMEDUSA/ALLARC/1980/grid_W_19800110.nc")

