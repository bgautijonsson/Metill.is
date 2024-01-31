library(eurostat)
library(tidyverse)
library(here)


litur_island <- "#08306b"

litur_danmork <- "#e41a1c"

litur_finnland <- "#3690c0"

litur_noregur <- "#7f0000"

litur_svithjod <- "#fd8d3c"

litur_luxemborg <- "black"

litur_total <- "#005824"

litur_annad <- "#737373"

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
    land = "Samtals"
  )

data_hist <- data_hist |> 
  bind_rows(
    data_hist_total
  ) |> 
  pivot_longer(c(total, total_non_ukr, asylum_applicants)) 



d <- here(cache_dir, "raw_data.csv") |> 
  read_csv()


d_2023 <- d |> 
  filter(
    year(time) == 2023,
    name %in% c("grants", "asylum_applicants_non_ukraine", "positive_decisions")
  ) |> 
  mutate(
    name = fct_recode(
      name,
      "total" = "grants",
      "total_non_ukr" = "positive_decisions",
      "asylum_applicants" = "asylum_applicants_non_ukraine"
    )
  ) |> 
  select(-gdp, -per_pop_cumsum, -per_gdp)



end_date <- d_2023 |> 
  drop_na() |> 
  summarise(
    min_date = min(time),
    max_date = max(time),
    n_obs = n(),
    .by = c(land, name, pop)
  ) |> 
  summarise(
    start_date = max(min_date),
    end_date = min(max_date),
    n_obs = min(n_obs),
    .by = c(land, pop)
  ) |> 
  pull(end_date) |> 
  min()


d_2023 <- d_2023 |> 
  filter(
    time <= end_date
  ) |> 
  group_by(land, name, pop) |> 
  summarise_at(
    vars(value, per_pop),
    sum
  ) |> 
  ungroup() |> 
  mutate(
    time = clock::date_build(2023),
    end_date = end_date
  ) |> 
  pivot_longer(c(value, per_pop), names_to = "type") |> 
  pivot_wider() |> 
  mutate(
    total = total + total_non_ukr
  ) |> 
  pivot_longer(c(asylum_applicants:total_non_ukr)) |> 
  pivot_wider(names_from = type)


data_hist <- data_hist |> 
  bind_rows(
    d_2023
  ) |> 
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
      land == "Samtals" ~ litur_total,
      TRUE ~ litur_annad
    ),
    linewidth = 1 * (land == "Ísland"),
    size = as_factor(linewidth)
  ) |> 
  filter(
    land != "Bretland"
  ) |> 
  arrange(land, time)

data_hist |> 
  write_csv(here(cache_dir, "data_hist.csv"))

#### Pretty data raw counts ####

data_hist |> 
  select(time, land, name, value) |> 
  pivot_wider() |> 
  rename(
    "Fjöldi verndarveitinga(bæði hæli og tímabundin vernd)" = total,
    "Fjöldi hælisveitinga" = total_non_ukr,
    "Fjöldi umsókna um hæli" = asylum_applicants,
    "Dagsetning" = time,
    "Land" = land
  ) |> 
  write_csv(
    here(cache_dir, "data_hist_pretty_rawcounts.csv")
  )


#### Pretty data per pop ####

data_hist |> 
  select(time, land, name, value = per_pop) |> 
  pivot_wider() |> 
  rename(
    "Fjöldi verndarveitinga(bæði hæli og tímabundin vernd)" = total,
    "Fjöldi hælisveitinga" = total_non_ukr,
    "Fjöldi umsókna um hæli" = asylum_applicants,
    "Dagsetning" = time,
    "Land" = land
  ) |> 
  write_csv(
    here(cache_dir, "data_hist_pretty_perpop.csv")
  )
