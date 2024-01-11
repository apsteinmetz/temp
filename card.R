library(tidyverse)

card_choices <- read_csv("card_choices.csv",col_types = "ii")
card_choices <- edit(card_choices)
card_choices <- card_choices %>% gather("item","choice") %>% mutate(item = as_factor(item))
ggplot(card_choices,aes(choice,fill=item)) + geom_histogram(position = "dodge",binwidth = 1)


