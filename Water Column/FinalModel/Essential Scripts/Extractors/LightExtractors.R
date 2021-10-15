
files<-list.files("/Users/michael/NEMOMEDUSA/light", recursive=TRUE, full.names = TRUE) %>% 
  as.data.frame() %>% 
  rename(filename='.') %>% 
  mutate(Year=str_sub(filename, end=-4, start=-7)) %>% 
  mutate( Type=case_when(str_detect(filename, 'history')~"history",
                         str_detect(filename, 'rcp85')~"rcp85",
                         T~"untargeted")
  )  %>% 
  filter(Type!="untargeted") 


lightdata<-lapply(1:nrow(files), function(x) get_Light(files[[1]][x], -33, 61, T))

Years<-seq(1980,2099,1)
date<-seq(as.Date("05/01"), ("30/12"), "days")

names(lightdata)<-Years

lightdata1<-do.call(cbind, lightdata)









