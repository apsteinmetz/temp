#generate OHLC for 3 stocks
library(tidyverse)
library(tidyquant)
library(gridExtra)

#get prices 
my_prices  <- tq_get(c("AAPL","TSLA","NFLX"), get = "stock.prices", from = " 2017-09-20")
my_prices <- my_prices %>% group_by(symbol)

# Make OHLC ranges
year_range <- my_prices %>% 
  summarise(high=max(close),low=min(close),open=first(close),close=last(close))

day_range <- my_prices %>% 
  summarise(high=last(high),low=last(low),open=last(open),close=last(close))

#set up plot theme
theme_set(theme_minimal() + 
            theme(axis.text.y=element_blank(),
                  axis.title.x=element_blank(),
                axis.ticks.x=element_blank()))
# function to plot either intraday or 52-week
OHLC_plot<- function(SOHLC,label){
  line_width <- 5
  ggplot(SOHLC, aes(x="stock",y=close)) + 
    geom_segment(aes(x="stock",
                     xend="stock",
                     y=low,
                     yend=high),
                 size=line_width,
                 color="lightgrey")+
    geom_segment(aes(x="stock",
                     xend="stock",
                     y=open,
                     yend=close),
                 color="blue",
                 size=line_width)+
    geom_point(size=line_width * 2,color="blue") + 
    labs(title=label,
         x="",y="",
         caption="source: Yahoo") +
    
    coord_flip()+
    facet_wrap(.~symbol,scales="free",ncol=1)
}  

p1 <-OHLC_plot(year_range,"52 Week")
p2 <-OHLC_plot(day_range,"Intraday")

#normalize prices
my_prices <- my_prices %>% mutate(rel_close=close-first(close))
#plot sparklines
p3<-ggplot(my_prices,aes(date,rel_close,fill=symbol,color=symbol))+ 
  geom_area() + geom_line()+
  geom_hline(yintercept = 0)+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")+
  #LAZY so I colored manually.  You would want to dynamically set gain or loss color
  scale_fill_manual(values=c("AAPL"="lightgreen","NFLX"="lightgreen","TSLA"="pink"))+
  scale_color_manual(values=c("AAPL"="darkgreen","NFLX"="darkgreen","TSLA"="red"))+
  labs(title="Year Range")+
  facet_wrap(.~symbol,ncol=1,scales="free")
#arrange plots on a panel
grid.arrange(p1,p2,p3,layout_matrix=rbind(c(1,1,2,2,3)))
