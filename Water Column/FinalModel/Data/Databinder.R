library(tidyverse)

realdepthlist<-function(NEMOfile){
  Layer<-seq(1, 75, 1)
  nc<-ncdf4::nc_open(NEMOfile)
  Realdepth<-ncdf4::ncvar_get(nc, "deptht")
  ncdf4::nc_close(nc)
  d<-as.data.frame(Layer)
  depth<-as.data.frame(cbind(Layer, Realdepth))
  return(depth)
}


realdepthlistw<-function(NEMOfile){
  Layer<-seq(1, 75, 1)
  nc<-ncdf4::nc_open(NEMOfile)
  Realdepth<-ncdf4::ncvar_get(nc, "depthw")
  ncdf4::nc_close(nc)
  d<-as.data.frame(Layer)
  depth<-as.data.frame(cbind(Layer, Realdepth))
  return(depth)
}
Depth.dat<-realdepthlist("/Users/michael/NEMOMEDUSA/ALLARC/2072/grid_T_20720110.nc")  
Depth.dat.w<-realdepthlistw("/Users/michael/NEMOMEDUSA/ALLARC/2072/grid_W_20720615.nc")

IrmingerData<-list.files("/Users/michael/Desktop/PhD/Research Methods/Water Column Models/Water Column/FinalModel/Data/IrmingerBasin", full.names = TRUE) %>% 
  map(readRDS) %>% 
  bind_rows() %>% 
  cbind(Depth=Depth.dat.w[,2]) %>% 
  cbind(Depthmid=Depth.dat[,2])

  
  
  IrmingerData$Layer<-as.double(IrmingerData$Layer)
  str(IrmingerData$Layer)
  
  
  unique() %>% 
  filter(!.$Salinity==0)
 

bind$Vertical_diffusivity<- ifelse(bind$Vertical_diffusivity>0.15, 0.15, bind$Vertical_diffusivity) 
bind1<-filter(bind, bind$Depth==2) 
  write.csv(bind1,file=paste0("Surface.csv"))





nc<-ncdf4::nc_open("/Volumes/G-DRIVE/ALLARC/1986/grid_W_19860210.nc")

nc$dim$depthw


