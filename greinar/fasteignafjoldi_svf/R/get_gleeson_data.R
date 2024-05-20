library(tidyverse)
library(zoo)
library(arrow)

d_gleeson <- read_csv("https://raw.githubusercontent.com/jgleeson/PublicHouse/main/dataset.csv") |> 
  group_by(area_name, variable) |> 
  complete(year = min(year):max(year)) |> 
  mutate(value = na.approx(value, na.rm = FALSE)) |>  
  fill(full_area_name:grouping, source_org:source_title, frequency:notes) |> 
  ungroup() |> 
  filter(
    area_level == "country"
  ) |> 
  summarise(
    value = sum(value),
    .by = c(year, variable, country_name)
  ) |> 
  pivot_wider(names_from = variable, values_from = value) |> 
  rename(country = country_name) |> 
  mutate(
    new_dwellings = (dwellings - lag(dwellings)) / (year - lag(year)),
    new_by_pop = new_dwellings / population * 1000,
    rolling = slider::slide_index_dbl(new_by_pop, year, sum, .before = 9),
    .by = country
  ) |> 
  drop_na()

d_gleeson |> write_parquet("greinar/fasteignafjoldi/data/data_gleeson.parquet")