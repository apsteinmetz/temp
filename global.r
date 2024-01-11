library("shiny")
library(dplyr)
load("~/R Projects/wfmu_explorer/playlists.Rdata")
selectList<-playlists %>% 
  ungroup() %>% 
  select(ArtistToken) %>% 
  unique() %>% 
  pull(ArtistToken)
