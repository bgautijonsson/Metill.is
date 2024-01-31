library(tidyverse)
library(eurostat)
library(here)

cache_dir <- here("greinar", "innflytjendur", "data")

d <- get_eurostat(
  "migr_pop3ctb",
  filters = list(
    age = "TOTAL",
    sex = "T",
    c_birth = c("TOTAL", "NAT", "FOR")
  )
)  |> 
  janitor::remove_constant() |> 
  label_eurostat() |> 
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |> 
  select(-geo) |> 
  mutate(
    c_birth = fct_recode(
      c_birth,
      "Erlendis" = "Foreign country",
      "Innanlands" = "Reporting country",
      "Samtals" = "Total"
    )
  ) |> 
  select(land, dags = time, cbirth = c_birth, value = values) |> 
  pivot_wider(names_from = cbirth) |> 
  janitor::clean_names() |> 
  mutate(
    perc_innfl = erlendis / samtals
  ) |> 
  drop_na() |> 
  filter(
    !land %in% c("Liechtenstein", "Lúxemborg", "England"),
    year(dags) >= 2011
  )

d |> 
  write_csv(
    here(cache_dir, "cbirth_eurostat.csv")
  )
