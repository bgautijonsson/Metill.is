library(tidyverse)
library(eurostat)
library(here)

cache_dir <- here("greinar", "innflytjendur", "data")

atvinna <- get_eurostat(
  "lfsq_ergan",
  filters = list(
    citizen = list("FOR", "TOTAL", "NAT"),
    sex = "T",
    age = "Y20-64"
  ),
  cache = TRUE,
  cache_dir = "data"
) 

atvinna <- atvinna |>
  label_eurostat() |>
  select(geo, citizen, time, values) |>
  rename(country = geo)


atvinna <- atvinna |> 
  mutate(
    country = ifelse(str_detect(country, "Germany"), "Germany", country)
  ) |>
  inner_join(
    metill::country_names()
  ) |>
  mutate(
    citizen = fct_recode(
      citizen,
      "Samtals" = "Total",
      "Erlent" = "Foreign country",
      "Innlent" = "Reporting country"
    )
  ) |>
  select(land, vinnuafl = citizen, dags = time, hlutf_virk = values) |>
  mutate(
    hlutf_virk = hlutf_virk / 100
  ) |> 
  drop_na() |> 
  filter(
    !land %in% c("Liechtenstein", "Lúxemborg", "England"),
    year(dags) >= 2011,
    vinnuafl != "Samtals"
  ) |> 
  pivot_wider(names_from = vinnuafl, values_from = hlutf_virk) |> 
  janitor::clean_names() |> 
  filter(
    !any(is.na(erlent)),
    .by = land
  ) 

atvinna |> 
  write_csv(
    here(cache_dir, "jobmarket_working_citizen_eurostat.csv")
  )
