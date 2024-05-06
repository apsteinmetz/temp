# gen market returns
library(tidyverse)
library(slider)

# "https://www.randomlists.com/random-animals?show_images=false&dup=false&qty=100"
animals_raw  <- tribble(
  ~animal,
  "parakeet",
  "bison",
  "badger",
  "panda",
  "dormouse",
  "canary",
  "porcupine",
  "weasel",
  "monkey",
  "panther",
  "horse",
  "waterbuck",
  "muskrat",
  "doe",
  "gila_monster",
  "mustang",
  "gemsbok",
  "ferret",
  "musk_deer",
  "polar_bear",
  "duckbill_platypus",
  "highland_cow",
  "dugong",
  "puma",
  "rhinoceros",
  "kitten",
  "turtle",
  "dingo",
  "wombat",
  "walrus",
  "mouse",
  "shrew",
  "chameleon",
  "rooster",
  "zebu",
  "armadillo",
  "impala",
  "prairie_dog",
  "chinchilla",
  "chimpanzee",
  "lamb",
  "goat",
  "yak",
  "beaver",
  "capybara",
  "blue_crab",
  "hartebeest",
  "octopus",
  "aardvark",
  "grizzly_bear",
  "parrot",
  "gorilla",
  "chamois",
  "cheetah",
  "warthog",
  "lovebird",
  "peccary",
  "ape",
  "guinea_pig",
  "marten",
  "elephant",
  "eagle_owl",
  "ground_hog",
  "tiger",
  "meerkat",
  "buffalo",
  "starfish",
  "lion",
  "porpoise",
  "pig",
  "musk-ox",
  "mare",
  "stallion",
  "hog",
  "wildcat",
  "otter",
  "coyote",
  "koala",
  "basilisk",
  "toad",
  "jerboa",
  "mongoose",
  "pony",
  "pronghorn",
  "dung_beetle",
  "orangutan",
  "salamander",
  "hyena",
  "moose",
  "frog",
  "springbok",
  "budgerigar",
  "eland",
  "elk",
  "snake",
  "cat",
  "dog",
  "mountain_goat",
  "okapi",
  "gopher")


set.seed(123)
periods <- 1000
year <- 1:periods

returns <- rnorm(periods, mean = 0.07, sd = 0.01)
market_returns <- tibble(year = year, market_return = returns)
market_returns
# plot distribution of returns
market_returns |> ggplot(aes(x = market_return)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribution of Market Returns",
       x = "Returns",
       y = "Frequency")

# generate 10 years of returns for 100 funds
fund_count <- 100
funds <- animals_raw |> 
  transmute(fund = paste0(animal,"_fund"))

fundret <- funds %>% 
  crossing(market_returns) |> 
  mutate(fund_return_1y = rnorm(periods*fund_count, mean = market_return, sd = 0.02)) %>% 
  #compute rolling 3-year return
  group_by(fund) %>%
  mutate(cumret = cumprod(1 + fund_return_1y)) %>% 
  mutate(fund_return_3y =  cumret / lag(cumret,3)-1) %>%
  # compute rolling 5-year return
  mutate(fund_return_5y =  cumret / lag(cumret,5)-1)

  
fundret


# plot distribution of returns for funds
fundret |>
  filter(fund == "aardvark_fund") |> 
  ggplot(aes(x = fund_return_1y)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribution of fund Returns",
       x = "Returns",
       y = "Frequency")

fundret  |> ggplot(aes(x = fund_return_1y)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribution of fund Returns",
       x = "Returns",
       y = "Frequency")
# rank funds for each year

funds_ranked <- fundret %>% 
  group_by(year) %>% 
  # get percentile ranking. lower is better
  mutate(rank_1y = round(100*(1-percent_rank(fund_return_1y)))) %>% 
  mutate(rank_3y = round(100*(1-percent_rank(fund_return_3y)))) %>% 
  mutate(rank_5y = round(100*(1-percent_rank(fund_return_5y)))) %>% 
  # tag funds in the second quartile
  mutate(second_quartile = ifelse((rank_1y < 51) & (rank_1y > 25), 1, 0)) %>%
  group_by(fund) %>%
  mutate(consecutive_years_3 = slide_dbl(second_quartile,sum, .before =2)) %>%
  mutate(consecutive_years_5 = slide_dbl(second_quartile,sum, .before =4)) %>% 
  na.omit()

test_3 <- funds_ranked %>% 
  # ungroup() %>%
  filter(consecutive_years_3 == 3) %>% 
  summarise(mean_rank_1y = mean(rank_1y), mean_rank_3y = mean(rank_3y))

test_3 %>% 
  ungroup() %>% 
  summarise(mean(mean_rank_1y), mean(mean_rank_3y))

test_5 <- funds_ranked %>% 
  filter(consecutive_years_5 == 5) %>% 
  summarise(mean_rank_1y = mean(rank_1y),mean_rank_5y = mean(rank_5y))

test_5 %>% 
  ungroup() %>% 
  summarise(mean(mean_rank_1y), mean(mean_rank_5y))
