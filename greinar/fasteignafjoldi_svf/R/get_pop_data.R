library(tidyverse)
library(eurostat)

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


d |> 
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
  select(country = geo, year, population_adult) |>
  write_parquet("greinar/fasteignafjoldi/data/data_pop.parquet")
