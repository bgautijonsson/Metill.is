library(tidyverse)
library(arrow)
library(metill)
library(rjson)

get_tables <- function(data, years) {
  tibble(
    ar = years,
    svfn = data$svfn,
    sveitarfelag = data$name,
    fjolbyli_fjoldi = data$fjolbyli_fjoldi,
    fjolbyli_medaltal = data$fjolbyli_medaltal,
    fjolbyli_midgildi = data$fjolbyli_midgildi,
    serbyli_fjoldi = data$serbyli_fjoldi,
    serbyli_medaltal = data$serbyli_medaltal,
    serbyli_midgildi = data$serbyli_midgildi,
  ) |> 
    pivot_longer(c(-svfn, -sveitarfelag, -ar), names_to = c("tegund", "breyta"), names_sep = "_") |> 
    mutate(value = ifelse(value == "None", "0", value)) |> 
    pivot_wider(names_from = breyta, values_from = value)
}

init_db()

d <- fromJSON(file = "https://talnaefni.fasteignaskra.is/talnaefni/v1/staerdibudasveitarfelog")

d <- d$sveitarfélög |> 
  map(get_tables, years = d$date) |> 
  reduce(bind_rows) |> 
  mutate_at(vars(ar, svfn, fjoldi, medaltal, midgildi), parse_number) |> 
  select(ar, sveitarfelag, tegund, fjoldi)

d <- d |>
  count(ar, sveitarfelag, wt = fjoldi, name = "fjoldi") |>
  arrange(ar, sveitarfelag) |>
  group_by(sveitarfelag) |>
  mutate(cum_fjoldi = cumsum(fjoldi)) |>
  ungroup()

mannfjoldi <- mtl_mannfjoldi_svf() |> 
  collect() |> 
  mutate(
    vinnualdur = ifelse((aldur >= 20) & (aldur <= 64), 1, 0),
    heild = 1,
    fullordin = ifelse(aldur >= 20, 1, 0)
  ) |>
  group_by(sveitarfelag, ar) |>
  summarise(
    mannfjoldi_vinnualdur = sum(mannfjoldi * vinnualdur),
    mannfjoldi_fullordin = sum(mannfjoldi * fullordin),
    mannfjoldi = sum(mannfjoldi * heild)
  )


d <- d |>
  left_join(
    mannfjoldi,
    by = c("ar", "sveitarfelag")
  ) |> 
  group_by(sveitarfelag) |> 
  fill(mannfjoldi_vinnualdur, mannfjoldi_fullordin, mannfjoldi, .direction = "down") |> 
  ungroup() |> 
  drop_na()

d |> 
  summarise(
    new_dwellings = sum(fjoldi),
    dwellings = sum(cum_fjoldi),
    population = sum(mannfjoldi),
    .by = ar
  ) |> 
  rename(year = ar) |> 
  mutate(
    country = "Iceland",
    new_by_pop = new_dwellings / population * 1000,
    rolling = slider::slide_index_dbl(new_by_pop, year, sum, .before = 9),
    country = "Iceland"
  ) |> 
  write_parquet("greinar/fasteignafjoldi/data/data_iceland.parquet")
