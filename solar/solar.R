# compare solar to weather data at lake
library(tidyverse)
library(googlesheets4)
library(pdftools)


# load all csv files in 'data' folder and combine them into one dataframe
setwd("~/R Projects/temp/solar")
powerwall_raw <- list.files("data", full.names = TRUE) |> 
  enframe() |> 
  filter(str_detect(value,"powerwall")) |> 
  pull(value) |>
  map_dfr(read_csv) |> 
  distinct()

powerwall <- powerwall_raw |> 
  janitor::clean_names() |> 
  select(!contains("_m_wh")) |> 
  rename_with(~str_remove(.x,"_k_wh")) |>
  mutate(date = as.Date(date_time),.before = date_time) |>
  select(-date_time) |> 
  filter(!is.na(solar_energy)) |>
  arrange(date) |> 
  mutate(net_to_grid = solar_energy - home)
powerwall

powerwall_long <- powerwall |> 
  pivot_longer(cols = !date,names_to = "source",values_to = "kWh")
  
# plot solar energy
powerwall_long |> 
  filter(date >= "2023-04-30") |>
  filter(source %in% c("solar_energy","net_to_grid","home")) |>
  ggplot(aes(x=date,y=kWh,color = source)) +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Solar Energy", x = "Date", y = "Solar Energy (kWh)")

powerwall |> 
  filter(date >= "2023-04-30") |>
  summarise(across(where(is.numeric),\(x) sum(x,na.rm = TRUE)))

# extract table from pdf
list.files("data", full.names = TRUE) |> 
  enframe() |> 
  filter(str_detect(value,"Resi")) |> 
  pull(value) |> 
  pdftools::pdf_text()

|> 
  str_split("\n") |> 
  unlist() |> 
  as_tibble() |> 
  filter(str_detect(value,"[0-9]")) |> 
  separate(value,into = c("date","time","temp","humidity","wind_speed","wind_dir"),sep = "\\s+") |> 
  mutate(date = as.Date(date),time = hms::as_hms(time)) |> 
  select(date,time,temp,humidity,wind_speed,wind_dir) |> 
  arrange(date,time)