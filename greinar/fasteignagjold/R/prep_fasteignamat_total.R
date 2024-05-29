library(tidyverse)
library(readxl)
library(purrr)
library(here)
library(ggh4x)
library(glue)

read_fasteignagjold <- function(year) {
  
  read_excel(
    here("greinar", "fasteignagjold", "data", "arbok", glue("rbok-{year}-toflur.xlsx")),
    sheet = "Tafla 14",
    skip = 8,
    col_names = c(
      "svfn",
      "sveitarfelag",
      "utsvar",
      "fskattur_a",
      "fskattur_b",
      "fskattur_c",
      "fraveitugjald",
      "vatnsgjald",
      "sorphreinsun_tunnugjald",
      "sorphreinsun_eydingargjald",
      "lodarleiga_ibudir",
      "lodarleiga_fyrirtaeki",
      "fjoldi_gjalda"
    ),
    col_types = rep("text", 13)
  ) |>
    mutate(
      sveitarfelag = str_replace_all(sveitarfelag, "[0-9]+\\)", "") |>
        str_replace_all("1 \\)", "") |>
        str_squish()
    ) |>
    drop_na(svfn) |>
    select(sveitarfelag, fskattur_a, fraveitugjald, vatnsgjald) |>
    mutate_at(
      vars(fskattur_a, fraveitugjald, vatnsgjald),
      ~ ifelse(str_detect(., "kr") | is.na(.), "0", as.character(.)) |>
        str_replace(",", "\\.") |>
        parse_number()
    ) |>
    mutate(fasteignamat = (fskattur_a + fraveitugjald + vatnsgjald) / 100) |>
    select(sveitarfelag, fasteignamat) |> 
    mutate(year = year)
}

d <- map_df(2018:2024, read_fasteignagjold) |> 
  mutate(
    sveitarfelag = case_when(
      str_detect(sveitarfelag, "Seltjarn") ~ "Seltjarnarnesbær",
      str_detect(sveitarfelag, "Reykjav") ~ "Reykjavíkurborg",
      TRUE ~ sveitarfelag
    )
  ) |> 
  mutate(
    n = n(),
    .by = sveitarfelag
  ) |> 
  filter(n == max(n)) 

d |> 
  # semi_join(
  #   here("greinar", "fasteignagjold", "data", "svf_pop.csv") |>
  #     read_csv()
  # ) |>
  write_csv(
    here("greinar", "fasteignagjold", "data", "fasteignamat_total_clean.csv")
  )
