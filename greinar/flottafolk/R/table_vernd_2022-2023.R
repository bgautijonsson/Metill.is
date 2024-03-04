data_hist |> 
  filter(
    land != "Samtals",
    year(time) >= 2022
  ) |> 
  select(time, land, name, per_pop) |>
  pivot_wider(values_from = per_pop) |> 
  mutate(
    asylum_applicants_non_ukr = asylum_applicants,
    asylum_applicants = asylum_applicants + (total - total_non_ukr)
  ) |> 
  group_by(land) |> 
  summarise_at(
    vars(-time),
    mean
  ) |> 
  mutate(
    l = janitor::make_clean_names(land),
    colour = case_when(
      land == "Ísland" ~ litur_island,
      land == "Danmörk" ~ litur_danmork,
      land == "Finnland" ~ litur_finnland,
      land == "Noregur" ~ litur_noregur,
      land == "Svíþjóð" ~ litur_svithjod,
      land == "Samtals" ~ litur_total,
      TRUE ~ litur_annad
    ),
    textsize = if_else(colour == litur_annad, 12, 12),
    fontweight = if_else(colour == litur_annad, "normal", "bold"),
    land = glue("<span style='color:{colour};font-size:{textsize}pt;font-weight:{fontweight};'>{land}</span>")
  ) |> 
  arrange(l) |> 
  select(1:5) |> 
  gt() |> 
  tab_header(
    title = "Samantekt á verndarkerfum Evrópulanda (2022 - 2023)",
    subtitle = "Tölur sýndar sem fjöldi á 100.000 íbúa móttökulands"
  ) |> 
  tab_source_note(
    "Tafla eftir @bggjonsson hjá metill.is byggð á gögnum Eurostat um fólksflutninga: https://metill.is/greinar/flottafolk\nGögn og kóði: https://github.com/bgautijonsson/Metill.is/tree/master/greinar/flottafolk"
  ) |> 
  cols_label(
    land = "",
    total = "Samtals",
    total_non_ukr = "Án Úkraínu",
    asylum_applicants = "Samtals",
    asylum_applicants_non_ukr = "Án Úkraínu"
  ) |> 
  tab_spanner(
    label = md("**Verndarveitingar**"), columns = 2:3
  ) |> 
  tab_spanner(
    label = md("**Umsóknir**"), columns = 4:5
  ) |> 
  fmt_number(decimals = 0) |> 
  fmt_markdown(land) |> 
  gt_color_rows(
    columns = -land,
    domain = NULL,
    palette = "Greys"
    ) |> 
  opt_vertical_padding(0.7) |> 
  gtsave(
    filename = "Greinar/flottafolk/Figures/vernd_2022-2023.png",
    zoom = 1,
    delay = 0.1,
    expand = 50,
    vwidth = 600,
    vheight = 2 * 600
  )
