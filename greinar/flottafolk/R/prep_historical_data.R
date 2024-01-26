library(eurostat)
library(tidyverse)
library(here)

cache_dir <- here("greinar", "flottafolk", "data")
update_cache <- TRUE

#### Hælisleitendur ####
asylum_applicants_hist <- get_eurostat(
  "migr_asyappctza",
  filters = list(
    citizen = list("TOTAL", "UA"),
    sex = "T",
    age = "TOTAL",
    asyl_app = "ASY_APP"
  )
)

#### Ákvarðanir í hælisumsóknum ####
decisions_hist <- get_eurostat(
  "migr_asydcfsta",
  filters = list(
    citizen = list("TOTAL", "UA"),
    sex = "T",
    age = "TOTAL"
  )
)

#### Veitingar tímabundinnar verndar ####
temporary_protection_hist <- get_eurostat(
  "migr_asytpfa",
  filters = list(
    citizen = "TOTAL",
    age = "TOTAL",
    sex = "T"
  )
)  |> 
  select(-citizen, -age, -sex, -freq, -unit) |> 
  label_eurostat() |> 
  rename(
    granted_temporary_protection = values
  )

#### Mannfjöldi ####
pop_hist <- get_eurostat(
  "demo_pjan",
  filters = list(
    age = "TOTAL",
    sex = "T"
  )
)

#### Sameining gagna ####

asylum_applicants_hist <- asylum_applicants_hist |> 
  janitor::remove_constant() |> 
  label_eurostat() |> 
  pivot_wider(names_from = citizen, values_from = values) |> 
  mutate(
    asylum_applicants = Total - Ukraine
  ) |> 
  select(-Ukraine, -Total)

decisions_hist <- decisions_hist |> 
  janitor::remove_constant() |> 
  label_eurostat()

pop_hist <- pop_hist |> 
  janitor::remove_constant() |> 
  label_eurostat()

data_hist <- decisions_hist |> 
  rename(
    decisions = values
  ) |> 
  inner_join(
    pop_hist |> rename(pop = values),
    by = join_by(geo, time)
  ) |> 
  pivot_wider(names_from = decision, values_from = decisions) |> 
  janitor::clean_names() |> 
  select(citizen:pop, total_positive_decisions) |> 
  pivot_wider(names_from = citizen, values_from = total_positive_decisions) |> 
  left_join(
    temporary_protection_hist,
    by = join_by(geo, time)
  ) |> 
  mutate(
    granted_temporary_protection = coalesce(granted_temporary_protection, 0),
    total = Total - Ukraine + granted_temporary_protection,
    total_non_ukr = Total - Ukraine
  ) |> 
  select(geo, time, pop, total, total_non_ukr) |> 
  inner_join(
    asylum_applicants_hist,
    by = join_by(geo, time)
  ) |> 
  inner_join(
    metill::country_names(),
    by = join_by(geo == country)
  ) |> 
  filter(land != "Kýpur") |> 
  select(-geo)

data_hist_total <- data_hist |> 
  drop_na() |> 
  group_by(time) |> 
  summarise_at(
    vars(pop, total, total_non_ukr, asylum_applicants),
    sum
  ) |> 
  mutate(
    land = "Meðaltal"
  )

data_hist <- data_hist |> 
  bind_rows(
    data_hist_total
  ) |> 
  pivot_longer(c(total, total_non_ukr, asylum_applicants)) |> 
  mutate(
    per_pop = value / pop * 1e5,
    per_pop = coalesce(per_pop, 0),
    colour = case_when(
      land == "Ísland" ~ litur_island,
      land == "Danmörk" ~ litur_danmork,
      land == "Finnland" ~ litur_finnland,
      land == "Noregur" ~ litur_noregur,
      land == "Svíþjóð" ~ litur_svithjod,
      land == "Lúxemborg" ~ litur_luxemborg,
      land == "Meðaltal" ~ litur_total,
      TRUE ~ litur_annad
    ),
    linewidth = 1 * (land == "Ísland"),
    size = as_factor(linewidth)
  ) 

data_hist |> 
  write_csv(here(cache_dir, "data_hist.csv"))