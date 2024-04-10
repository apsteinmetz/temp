library(tidyverse)
library(rvest)

#import eml file as table

# assign downloads directory
downloads_dir<-"C:/Users/apste/Downloads"
name_list <- c("greg","tammy","christophe","peter","carrie","arikha","art","kathy")
item_list = c("Name","Weight","DOB","Nationality","Passport #","Passport Expiration Date","img_ref")

read_passport_rec <- function(name) {
  eml_file <- read_html(paste0(downloads_dir, "/", name, ".eml")) |>
    html_table() |>
    pluck(2) |>
    slice(6:29) |>
    # slice(6,8,18,19,22,28,29) |>
    mutate(item = str_remove_all(X1, "=20|=|\\*|\r|\n|\t")) |>
    mutate(nickname = name,.before = item) |>
    mutate(value = X2) |>
    select(nickname, item, value) |>
    bind_rows(tibble(nickname = name, item = "Weight", value = "160")) |>
    mutate(item = if_else(str_detect(item,"NameEXACTLY"), "Name", item)) |>
    mutate(item = if_else(str_detect(item,"of Birth"), "DOB", item)) |>
    mutate(item = if_else(str_detect(item,"Attach"), "img_ref", item)) |>
    filter(item %in% item_list)
}

# function using regex to extract  date of the form month name, day, year from string containing extraneous characters
extract_date <- Vectorize(function(text) {
  pattern <- "\\b(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},\\s+\\d{4}\\b"
  match <- str_match(text, pattern)
  if (!is.na(match[1, 1])) {
    date_parts <- str_split(match[1, 1], "\\s+")[[1]] |>
      str_remove(",")
    return(paste(date_parts[3], date_parts[1], date_parts[2], sep=" "))
  } else {
    return(NA)
  }
})



all_recs <- name_list |>
  #assign names to list items
  # set_names() |>
  map(read_passport_rec) |>
  bind_rows() |>
  pivot_wider(id_cols=nickname, names_from = item, values_from = value) |>
  mutate(Name = str_replace_all(Name,"\r\n\t\t\t"," ")) |>
  mutate(DOB = extract_date(DOB)) |>
  mutate(`Passport Expiration Date` = extract_date(`Passport Expiration Date`)) |>
  mutate(Nationality = "USA")


# write to csv
write_csv(all_recs, "manifest.csv")

