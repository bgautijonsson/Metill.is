library(tidyverse)
library(hagstofa)


url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/2_byggdir/sveitarfelog/MAN02005.px"


d <- hg_data(url) |> 
  filter(
    Aldur == "Alls",
    Kyn == "Alls",
    Sveitarfélag != "Alls",
    Ár == "2024"
  ) |> 
  collect()


d |> 
  janitor::remove_constant() |> 
  janitor::clean_names() |> 
  rename(pop = 2) |> 
  slice_max(order_by = pop, n = 30) |> 
  write_csv(
    here("greinar", "fasteignagjold", "data", "svf_pop.csv")
  )
