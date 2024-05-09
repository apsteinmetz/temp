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
pdf_fnames <- list.files("data", full.names = TRUE) |> 
  enframe() |> 
  filter(str_detect(value,"Resi")) |> 
  pull(value)


# determine the row boundaries of table
locate_row <- function(pdf_text,tag){
  mutate(pdf_text,row_number = row_number()) |>
    filter(str_detect(value,tag)) |> 
    pull(row_number) 
}

# make table from pdf text
make_table <- function(pdf_fname) {
  pdf_df <- pdftools::pdf_text(pdf_fname)  |>
    str_split("\n") |>
    unlist() |>
    as_tibble()
  table_start <- locate_row(pdf_df, "Day") + 1
  # "Refer" shows up twice. take second instance
  table_end <- locate_row(pdf_df, "Refer")[2] - 1
  energy_table <- pdf_df[table_start:table_end, ] |>
    # separate temp2 into columns where separator is two or more spaces
    separate(
      value,
      into = c("dummy", "date", "solar_energy", "home", "peak_gen_kw"),
      sep = "\\s{2,}"
    ) |>
    select(-dummy) |>
    na.omit() |>
    # convert date column in format "mmm dd, yyyy" to date type
    mutate(date = as.Date(date, "%b %d, %Y")) |>
    # convert other columns to numeric type
    mutate(across(where(is.character), as.numeric))
  return(energy_table)
}

make_table(temp)
