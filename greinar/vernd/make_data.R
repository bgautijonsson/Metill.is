library(eurostat)
library(tidyverse)

#### Applicants ####

applicants <- get_eurostat(
  "migr_asyappctza",
  cache = TRUE,
  update_cache = TRUE,
  cache_dir = "greinar/vernd/data-raw"
)  

d <- applicants |> 
  label_eurostat() |> 
  rename(time = TIME_PERIOD) |>
  filter(
    sex == "Total",
    age == "Total",
    asyl_app == "Asylum applicant",
    geo != "European Union - 27 countries (from 2020)"
  ) |>
  select(
    -freq, -sex, -unit, -age, -asyl_app
  ) |>
  rename(
    applicants = values
  )

d |> 
  write_csv("greinar/vernd/data/applicants.csv")


#### Population ####

pop <- get_eurostat(
  "demo_pjan",
  cache = TRUE,
  update_cache = TRUE,
  cache_dir = "greinar/vernd/data-raw"
)

d <- pop |> 
  label_eurostat() |> 
  rename(time = TIME_PERIOD) |>
  filter(
    sex == "Total",
    age == "Total",
  ) |>
  select(
    -freq, -sex, -unit, -age
  ) |>
  rename(pop = values)

d |> 
  write_csv(
    "greinar/vernd/data/pop.csv"
  )


#### Decisions ####

decisions <- get_eurostat(
  "migr_asydcfsta",
  cache = TRUE,
  update_cache = TRUE,
  cache_dir = "greinar/vernd/data-raw"
)

d <- decisions |> 
  label_eurostat() |> 
  rename(time = TIME_PERIOD) |>
  filter(
    sex == "Total",
    age == "Total",
    geo != "European Union - 27 countries (from 2020)",
    decision == "Total positive decisions"
  ) |>
  select(
    -freq, -sex, -unit, -age, -decision
  ) |>
  rename(
    decisions = values
  )

d |> 
  write_csv("greinar/vernd/data/decisions.csv")
