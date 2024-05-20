url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Samfelag/vinnumarkadur/vinnuaflskraargogn/VIN10032.px"

hg_dat <- hg_data(url) |> 
  filter(
    Aldur == "Alls",
    Kyn == "Alls",
    Lögheimili == "Alls"
  )

d <- hg_dat |>  
  collect() |> 
  janitor::clean_names()

d <- d |> 
  separate(manudur, into = c("ar", "manudur"), sep = "M", convert = TRUE) |> 
  mutate(dags = clock::date_build(ar, manudur)) |>  
  select(dags, kyn, atvinnugrein_balkar, starfandi, bakgrunnur) |>  
  drop_na() |>  
  janitor::remove_constant()

d |> 
  rename(
    atv = atvinnugrein_balkar
  ) |> 
  filter(
    atv != "Alls - Starfandi",
    bakgrunnur != "Alls",
    !year(dags) %in% c(2020, 2021)
  ) |> 
  janitor::remove_constant() |> 
  mutate(
    hlutf = starfandi / sum(starfandi),
    .by = c(dags, atv)
  ) |>
  filter(bakgrunnur == "Innflytjendur") |> 
  janitor::clean_names() |> 
  mutate(
    man = month(dags),
    ar = year(dags),
    atv = str_wrap(atv, 40)
  ) |> 
  mutate(
    mean = mean(hlutf),
    diff = hlutf / mean,
    .by = c(ar, atv)
  ) |> 
  ggplot(aes(man, diff)) +
  geom_hline(yintercept = 1, lty = 2, alpha = 0.4) +
  geom_line(aes(group = ar, col = ar)) +
  scale_x_continuous(
    breaks = 1:12
  ) +
  scale_y_continuous(
    breaks = breaks_pretty(),
    labels = function(x) hlutf(x - 1),
    trans = "log10"
  ) +
  scale_colour_distiller(palette = "RdBu") +
  facet_wrap("atv", scales = "free") +
  labs(
    title = "Árstíðarleitni í hlutfalli innflytjenda af öllum starfandi",
    subtitle = "Sýnt sem % munur frá meðaltali"
  )

d |> 
  rename(
    atv = atvinnugrein_balkar
  ) |> 
  filter(
    atv != "Alls - Starfandi",
    bakgrunnur != "Alls"
  ) |> 
  janitor::remove_constant() |> 
  mutate(
    hlutf = starfandi / sum(starfandi),
    .by = c(dags, atv)
  ) |>
  filter(bakgrunnur == "Innflytjendur") |> 
  janitor::clean_names() |> 
  mutate(
    man = month(dags),
    ar = year(dags),
    atv = str_wrap(atv, 40)
  ) |> 
  mutate(
    mean = mean(starfandi),
    diff = starfandi / mean,
    .by = c(ar, atv)
  ) |> 
  ggplot(aes(dags, hlutf)) +
  geom_line() +
  scale_y_continuous(
    breaks = breaks_pretty(),
    labels = label_hlutf()
  ) +
  scale_colour_distiller(palette = "RdBu") +
  facet_wrap("atv", scales = "free")



d |> 
  rename(
    atv = atvinnugrein_balkar
  ) |> 
  filter(
    atv != "Alls - Starfandi",
    bakgrunnur != "Alls",
    dags == max(dags),
    str_detect(atv, "^[A-Z]+ ")
  ) |> 
  janitor::remove_constant() |> 
  mutate(
    hlutf = starfandi / sum(starfandi),
    .by = c(atv)
  ) |>
  filter(bakgrunnur == "Innflytjendur") |> 
  janitor::clean_names() |> 
  select(atv, starfandi, hlutf) |> 
  arrange(desc(hlutf)) |> 
  gt() |> 
  tab_header(
    title = "Fjöldi og hlutfall innflytjenda meðal starfandi innan atvinnugreina"
  ) |> 
  cols_label(
    atv = "Atvinnugrein",
    starfandi = "Fjöldi (n)",
    hlutf = "Hlutfall (%)"
  ) |> 
  fmt_percent(
    columns = hlutf
  ) |> 
  fmt_integer(
    columns = starfandi
  )
