# gen market returns
library(tidyverse)
library(rvest)

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
n <- 10
year <- 1:n

returns <- rnorm(n, mean = 0.07, sd = 0.01)
market_returns <- tibble(year = year, market_return = returns)
market_returns
# plot distribution of returns
market_returns |> ggplot(aes(x = market_return)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribution of Market Returns",
       x = "Returns",
       y = "Frequency")

# generate 10 years of returns for 100 funds
n <- 100
funds <- animals_raw |> 
  transmute(fund = paste0(animal,"_fund"))
fundret <- funds %>% 
  crossing(market_returns) |> 
  mutate(fund_return = rnorm(1000, mean = market_return, sd = 0.02))
fundret


# plot distribution of returns for funds
fundret |> 
  filter(fund == "aardvark_fund") |> 
  ggplot(aes(x = fund_return)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribution of fund Returns",
       x = "Returns",
       y = "Frequency")

fundret  |> ggplot(aes(x = fund_return)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribution of fund Returns",
       x = "Returns",
       y = "Frequency")
# rank funds for each year
funds_ranked <- fundret %>% 
  group_by(year) %>% 
  mutate(rank = 101-rank(fund_return))
funds_ranked

funds_ranked |> 
  filter(fund == "frog_fund") |> 
  ggplot(aes(x = rank)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribution of fund Returns",
       x = "Ranks",
       y = "Frequency")

