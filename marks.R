library(tidyverse)
library(rvest)
library(ggplot2)

source_codes<-data_frame(code=c("india","rx","os","uk"),
                          source=c("India","Canada","Mauritius","UK"))
marks<-read_html("marks.html")
price_table<-html_nodes(marks,"table") %>% .[14]

raw_prices<- price_table %>% html_table() %>% .[[1]]
names(raw_prices)<-c("na","drug","generic","dose_raw","quantity","form","price")
prices<-raw_prices %>% 
  as_data_frame() %>% 
  mutate(generic=as.factor(generic)) %>% 
  mutate(dose=as.numeric(str_extract(dose_raw,"[0-9.]+"))) %>% 
  mutate(dose_unit=str_to_lower(str_extract(dose_raw,"[A-Za-z]+"))) %>% 
  separate(price,into=c("price","currency"),sep=" ") %>% 
  mutate(price=as.numeric(str_extract(price,"[0-9.]+"))) %>%
  mutate(price_per_mg=round(price/(dose*quantity),2)) %>% 
  select(-na,-dose_raw) %>%
  {.}

prices<-price_table %>% 
  html_nodes("img") %>% 
  html_attr("src") %>% 
  data_frame(images=.) %>% 
  filter(str_detect(images,".gif")) %>%
  mutate(code=str_extract(images,"(?<=\\/)([a-z])+(?=.gif)")) %>% 
  left_join(source_codes) %>% 
  select(source) %>% 
  bind_cols(prices) %>%
  mutate(source=as_factor(source)) %>% 
  arrange(price_per_mg)

ggplot(prices,aes(x=source,y=price_per_mg,color=generic,size=dose))+geom_jitter(width=0.15,height =0)
