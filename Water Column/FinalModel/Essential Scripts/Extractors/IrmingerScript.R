library(tidyverse)
library(data.table)
files<-list.files("/Users/michael/NEMOMEDUSA/ALLARC", recursive=TRUE, 
                  full.names = TRUE) %>% 
  as.data.frame() %>% 
  rename(filename='.') %>% 
  mutate(date=str_sub(filename, end=-4, start=-11))


head(files)
future::plan("multiprocess")




data<-filter(files, str_detect(filename, 'grid_W_')) %>% 
  furrr::future_pmap(VerVel, x=1021, y=173, .progress=TRUE) %>% 
  rbindlist()



data1<-filter(data, str_starts(data$Date,"2002"))



data1$VerticalVelocity[data1$VerticalVelocity>0.15]<-0.15
data2<-pivot_wider(data1, id_cols=Date, names_from = Depth,
                   values_from = VerticalVelocity)

write.csv(data2, "data2.csv")

max(data2[,-1])
data3<-data2[,-1]




datt<-NEMO_MEDUSA(files, "StrathE2E", 
                  out_dir = "/Users/michael/Desktop/untitled folder")


