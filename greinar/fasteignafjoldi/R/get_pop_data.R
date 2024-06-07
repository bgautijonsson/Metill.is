library(tidyverse)
library(eurostat)
library(arrow)

pop <- get_eurostat(
  "demo_pjan",
  filters = list(
    sex = "T"
  )
)

d <- pop |> 
  label_eurostat()


d <- d |> 
  filter(
    geo %in% c(
      "Iceland",
      "Norway",
      "Finland",
      "Denmark",
      "Sweden",
      "UK",
      "USA",
      "Germany",
      "France",
      "Austria",
      "Australia",
      "Switzerland",
      "Canada",
      "New Zealand",
      "Belgium",
      "Poland",
      "Netherlands",
      "Italy",
      "Ireland",
      "Spain",
      "Portugal"
    )
  ) |> 
  janitor::remove_constant() 


d <- d |> 
  filter(
    !age %in% c("Total", "Unknown", "Open-ended age class")
  ) |> 
  mutate(
    age = if_else(
      age == "Less than 1 year",
      "0",
      age
    ) |> 
      parse_number()
  ) |> 
  filter(
    age > 18
  ) |> 
  summarise(
    population_adult = sum(values, na.rm = T),
    .by = c(geo, time)
  ) |> 
  mutate(year = year(time)) |> 
  select(country = geo, year, population_adult)

library(hagstofa)

url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/1_yfirlit/Yfirlit_mannfjolda/MAN00101.px"


d_hg <- hg_data(url) |> 
  filter(
    parse_number(Aldur) > 18,
    Kyn == "Alls",
    parse_number(Ár) >= 2007
  ) |> 
  collect() |> 
  janitor::clean_names() |> 
  mutate_at(
    vars(aldur, ar),
    parse_number
  ) |> 
  rename(n = 4) |> 
  janitor::remove_constant() |> 
  summarise(
    population_adult = sum(n),
    .by = ar
  ) |> 
  rename(year = ar) |> 
  mutate(
    country = "Iceland"
  )

d |> 
  filter(country != "Iceland") |> 
  bind_rows(d_hg) |> 
  write_parquet("greinar/fasteignafjoldi/data/data_pop.parquet")
