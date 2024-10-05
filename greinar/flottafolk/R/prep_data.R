library(eurostat)
library(tidyverse)
library(here)

cache_dir <- here("greinar", "flottafolk", "data")
update_cache <- TRUE

#### Einstaklingar sem njóta tímabundinnar verndar ####
beneficiaries <- get_eurostat(
  "migr_asytpsm",
  cache = TRUE,
  update_cache = update_cache,
  cache_dir = cache_dir,
  filters = list(
    sex = "T",
    age = "TOTAL",
    citizen = "TOTAL"
  )
) |>
  select(-unit, -sex, -age, -citizen) |>
  label_eurostat()

#### Mannfjöldi ####
pop <- get_eurostat(
  "demo_pjan",
  cache = TRUE,
  update_cache = update_cache,
  cache_dir = cache_dir,
  filters = list(
    age = "TOTAL",
    sex = "T"
  )
) |>
  select(-unit, -age, -sex) |>
  label_eurostat()

#### Umsækjendur í bið eftir niðurstöðu hælisumsóknar ####
applicants <- get_eurostat(
  "migr_asypenctzm",
  cache = TRUE,
  update_cache = update_cache,
  cache_dir = cache_dir,
  filters = list(
    age = "TOTAL",
    sex = "T",
    citizen = c("TOTAL", "UA")
  )
) |>
  select(-sex, -age) |>
  janitor::remove_constant() |>
  label_eurostat() |>
  pivot_wider(names_from = citizen, values_from = values) |>
  janitor::clean_names() |>
  mutate(
    non_ukraine = total - ukraine
  ) |>
  rename(
    applicants_ukraine = ukraine,
    applicants_total = total,
    applicants_non_ukraine = non_ukraine
  )

#### Veitingar tímabundinnar verndar ####
grants <- get_eurostat(
  "migr_asytpfm",
  cache = TRUE,
  update_cache = update_cache,
  cache_dir = cache_dir,
  filters = list(
    sex = "T",
    age = "TOTAL",
    citizen = "TOTAL"
  )
) |>
  select(-unit, -citizen, -sex, -age) |>
  label_eurostat()

#### Ákvarðanir í hælisumsóknum ####
decisions <- get_eurostat(
  "migr_asydcfstq",
  cache = TRUE,
  update_cache = update_cache,
  cache_dir = cache_dir,
  filters = list(
    citizen = c("TOTAL", "UA"),
    sex = "T",
    age = "TOTAL",
    decision = c("TOTAL", "TOTAL_POS")
  )
) |>
  select(-sex, -age, -unit, -freq) |>
  label_eurostat() |>
  pivot_wider(names_from = citizen, values_from = values) |>
  mutate(values = Total - Ukraine) |>
  select(-Total, -Ukraine) |>
  pivot_wider(names_from = decision, values_from = values) |>
  janitor::clean_names() |>
  rename(total_decisions = total, positive_decisions = total_positive_decisions) |>
  mutate(
    percent_positive_decisions = positive_decisions / total_decisions
  )

decisions <- crossing(
  time = seq.Date(
    from = min(decisions$time),
    to = max(decisions$time),
    by = "1 month"
  ),
  geo = unique(decisions$geo)
) |>
  left_join(
    decisions,
    by = join_by(time, geo)
  ) |>
  arrange(time) |>
  group_by(geo) |>
  mutate_at(
    vars(total_decisions, positive_decisions, percent_positive_decisions),
    \(x) zoo::na.approx(x, na.rm = FALSE, maxgap = 5) / 3
  ) |>
  ungroup() |>
  mutate(
    percent_positive_decisions = percent_positive_decisions * 3
  ) |>
  drop_na()

#### Fjöldi hælisumsókna ####
asylum_applicants <- get_eurostat(
  "migr_asyappctzm",
  cache = TRUE,
  update_cache = update_cache,
  cache_dir = cache_dir,
  filters = list(
    sex = "T",
    age = "TOTAL",
    asyl_app = "ASY_APP",
    citizen = c("TOTAL", "UA")
  )
) |>
  select(-sex, -age, -freq, -unit, -asyl_app) |>
  drop_na() |>
  label_eurostat() |>
  rename(asylum_applicants = values) |>
  pivot_wider(names_from = citizen, values_from = asylum_applicants) |>
  janitor::clean_names() |>
  mutate(
    non_ukr = total - ukraine
  ) |>
  rename(
    asylum_applicants_total = total,
    asylum_applicants_ukraine = ukraine,
    asylum_applicants_non_ukraine = non_ukr
  )

#### Landsframleiðsla ####
gdp <- get_eurostat(
  "nama_10_gdp",
  filters = list(
    unit = "CP_MEUR",
    na_item = "B1GQ"
  )
) |>
  select(-freq, -unit, -na_item) |>
  label_eurostat() |>
  filter(year(time) == 2022) |>
  select(-time) |>
  rename(gdp = values)



#### Sameining gagna ####
d <- beneficiaries |>
  mutate(
    year = year(time)
  ) |>
  rename(beneficiaries = values) |>
  select(-freq) |>
  inner_join(
    pop |>
      select(-freq) |>
      drop_na(values) |>
      filter(
        time == max(time, na.rm = TRUE),
        .by = geo
      ) |>
      select(geo, pop = values),
    by = c("geo")
  ) |>
  inner_join(
    gdp,
    by = join_by(geo)
  ) |>
  full_join(
    applicants,
    by = join_by(geo, time)
  ) |>
  inner_join(
    grants |>
      select(-freq) |>
      rename(grants = values),
    by = join_by(geo, time)
  ) |>
  left_join(
    decisions,
    by = join_by(geo, time)
  ) |>
  left_join(
    asylum_applicants,
    by = join_by(geo, time)
  ) |>
  rename(country = geo) |>
  mutate(
    country = ifelse(str_detect(country, "Germany"), "Germany", country)
  ) |>
  inner_join(
    metill::country_names(),
    by = "country"
  ) |>
  select(
    -year, -country
  ) |>
  mutate(
    total_grants = grants + positive_decisions
  )

d_total <- d |>
  mutate(
    pop = max(pop, na.rm = T),
    .by = land
  ) |>
  group_by(time) |>
  summarise_at(
    vars(-land),
    \(x, ...) {
      perc_na <- mean(is.na(x))
      if (perc_na > 0.05) {
        return(NA)
      } else {
        return(sum(x, na.rm = TRUE))
      }
    }
  ) |>
  mutate(
    percent_positive_decisions = positive_decisions / total_decisions,
    land = "Samtals"
  ) |>
  mutate_at(
    vars(beneficiaries:total_grants),
    \(x) if_else(x == 0, NA, x)
  )

d <- d |>
  filter(land != "Kýpur") |>
  bind_rows(
    d_total
  ) |>
  pivot_longer(
    c(-land, -time, -pop, -gdp)
  ) |>
  group_by(land) |>
  mutate(pop = max(pop, na.rm = T)) |>
  ungroup() |>
  mutate(
    per_pop = value / pop * 1e5,
    per_gdp = value / gdp * 1e5
  ) |>
  mutate(
    per_pop_cumsum = cumsum(per_pop),
    .by = c(land, name)
  ) |>
  mutate(
    per_pop_cumsum = if_else(
      (name == "percent_positive_decisions"),
      value[name == "positive_decisions"] / value[name == "total_decisions"],
      per_pop_cumsum
    ),
    .by = c(land, time)
  )



d |>
  write_csv(here(cache_dir, "raw_data.csv"))

d |>
  mutate(
    per_pop = if_else(name == "percent_positive_decisions", value, per_pop)
  ) |>
  select(-per_pop, -pop, -per_gdp, -per_pop_cumsum) |>
  pivot_wider(names_from = name, values_from = value) |>
  select(
    "Dagsetning" = time,
    "Land" = land,
    "Fjöldi sem nýtur tímabundinnar verndar" = beneficiaries,
    "Nýjar veitingar á tímabundinni vernd" = grants,
    "Nýir umsækjendur um hæli" = asylum_applicants_non_ukraine,
    "Umsækjendur um hæli í bið eftir niðurstöðu" = applicants_non_ukraine,
    "Ákvarðanir teknar um hæli" = total_decisions,
    "Einstaklingum veitt hæli" = positive_decisions,
    "Hlutfall jákvæðra ákvarðana" = percent_positive_decisions
  ) |>
  write_csv(here(cache_dir, "timabundin_vernd_raw.csv"))

d |>
  mutate(
    per_pop = if_else(name == "percent_positive_decisions", value, per_pop)
  ) |>
  select(-value, -pop, -per_gdp, -per_pop_cumsum) |>
  pivot_wider(names_from = name, values_from = per_pop) |>
  select(
    "Dagsetning" = time,
    "Land" = land,
    "Fjöldi sem nýtur tímabundinnar verndar" = beneficiaries,
    "Nýjar veitingar á tímabundinni vernd" = grants,
    "Nýir umsækjendur um hæli" = asylum_applicants_non_ukraine,
    "Umsækjendur um hæli í bið eftir niðurstöðu" = applicants_non_ukraine,
    "Ákvarðanir teknar um hæli" = total_decisions,
    "Einstaklingum veitt hæli" = positive_decisions,
    "Hlutfall jákvæðra ákvarðana" = percent_positive_decisions
  ) |>
  write_csv(here(cache_dir, "timabundin_vernd_perpop.csv"))

d |>
  mutate(
    per_gdp = if_else(name == "percent_positive_decisions", value, per_gdp)
  ) |>
  select(-value, -pop, -per_pop, -per_pop_cumsum) |>
  pivot_wider(names_from = name, values_from = per_gdp) |>
  select(
    "Dagsetning" = time,
    "Land" = land,
    "Fjöldi sem nýtur tímabundinnar verndar" = beneficiaries,
    "Nýjar veitingar á tímabundinni vernd" = grants,
    "Nýir umsækjendur um hæli" = asylum_applicants_non_ukraine,
    "Umsækjendur um hæli í bið eftir niðurstöðu" = applicants_non_ukraine,
    "Ákvarðanir teknar um hæli" = total_decisions,
    "Einstaklingum veitt hæli" = positive_decisions,
    "Hlutfall jákvæðra ákvarðana" = percent_positive_decisions
  ) |>
  write_csv(here(cache_dir, "timabundin_vernd_pergdp.csv"))
