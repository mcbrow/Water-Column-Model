
remotes::install_github("Jack-H-Laverick/nemomedusR", force= TRUE)

library(tidyverse)
library(nemomedusR)
library(furrr)
library(tictoc)


plan("multiprocess")

tic()
  files<-list.files("/Users/michael/NEMOMEDUSA/ALLARC/2002", recursive=TRUE, full.names = TRUE) %>% 
  as.data.frame() %>% 
  rename(filename='.') %>% 
  mutate(date=str_sub(filename, end=-4, start=-11))%>%
  mutate( Type=case_when(str_detect(filename, 'grid_U_')~"grid_U_",
                         str_detect(filename, 'grid_T_')~"grid_T_",
                         str_detect(filename, 'grid_W_')~"grid_W_",
                         str_detect(filename, 'ptrc_T_')~"ptrc_T_",
                         str_detect(filename, 'grid_V_')~"grid_V_",
                         T~"untargeted")
          )  %>% 
  filter(Type!="untargeted") %>% 
  mutate(Year=str_sub(date, end=4, start=1),
         month=str_sub(date, end=6, start=5)) %>% 
  split( f = list(.$month, .$Year)) %>% 
  future_map(NEMO_MEDUSA, analysis = "1D", out_dir = "/Users/michael/Desktop/PhD/Research Methods/Water Column Models/Water Column/FinalModel/Data/IrmingerBasin", x=1021, y=173, .progress=TRUE) 
  toc()
  
 
 

  

