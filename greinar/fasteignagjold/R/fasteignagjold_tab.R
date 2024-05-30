library(tidyverse)
library(readxl)
library(metill)
library(here)
library(gt)
library(gtExtras)
theme_set(theme_metill())



read_excel(
  here("greinar", "fasteignagjold", "data", "arbok", "rbok-2024-toflur.xlsx"),
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
  mutate_if(is.numeric, \(x) x / 100) |> 
  mutate(fasteignagjold = (fskattur_a + fraveitugjald + vatnsgjald)) |> 
  arrange(fasteignagjold) |> 
  inner_join(
    read_excel(
      here("greinar", "fasteignagjold", "data", "Sveitarfélög eftir tegundum eigna 2025.xlsx")
    ) |>
      janitor::clean_names() |>
      filter(
        tegund_eigna == "Íbúðareignir"
      ) |>
      mutate(
        haekkun_verd = (fasteignamat_2025 - fasteignamat_2024) / fjoldi
      ) |>
      select(
        sveitarfelag, haekkun_verd
      )
  ) |> 
  mutate(
    haekkun_mat = fasteignagjold * haekkun_verd
  ) |> 
  arrange(haekkun_mat) |> 
  gt() |> 
  cols_label(
    sveitarfelag = "",
    fskattur_a = "Fasteignaskattur",
    fraveitugjald = "Fráveitugjöld",
    vatnsgjald = "Vatnsgjöld",
    fasteignagjold = "Fasteignagjöld",
    haekkun_verd = "Fasteignamat",
    haekkun_mat = "Fasteignagjöld"
  ) |> 
  cols_align(
    align = "center", 
    columns = -1
  ) |> 
  cols_width(
    sveitarfelag ~ px(250)
  ) |> 
  tab_spanner(
    columns = 2:4,
    label = "Undirliðir"
  ) |> 
  tab_spanner(
    2:5,
    label = "% af fasteignamati í álögðum gjöldum"
    ) |> 
  tab_spanner(
    columns = 6:7,
    label = "Meðalhækkun"
  ) |> 
  fmt_percent(
    2:5
  ) |> 
  fmt_currency(
    6:7, currency = "ISK", placement = "right"
  ) |> 
  gt_color_rows(
    columns = 2:7,
    palette = "Greys"
  ) |> 
  tab_header(
    "Sveitarfélögum landsins raðað eftir heildaráhrifum hærra fasteignamats á fasteignagjöld", 
    subtitle = md(
      str_c(
        '<div style="text-align: left"> ',
        "Samhliða fasteignaskatti innheimta sveitarfélög ýmis gjöld svo sem <b>fráveitu- og vatnsgjöld</b>, ",
        "<b>lóðaleigu</b> og <b>sorpgjald</b>. Saman kallast þessi gjöld gjarnan <b>fasteignagjöld</b>. ",
        "Sum þessara gjalda eru reiknuð hlutfallslega út frá fasteignamati fasteignar eða út frá stærð hennar í fermetrum, ",
        "og þetta getur verið mismunandi eftir sveitarfélagi",
        '</div>'
      )
    )
    
  ) |> 
  tab_footnote(
    footnote = ""
  ) |> 
  tab_source_note(
    md(
      str_c(
        "Fasteignagjöld: https://www.samband.is/verkefnin/fjarmal/tekjustofnar-sveitarfelaga/fasteignaskattur/",
        "<br>",
        "Fasteignamat: https://www.fasteignaskra.is/library/Skrar/fasteignamat/2025/Sveitarfélög%20eftir%20tegundum%20eigna.xlsx"
        
      )
    )
  ) |> 
  # gt_theme_538() |> 
  tab_style(
    locations = cells_title("title"),
    style = cell_text(font = "Lato", weight = 900, color = "#484D6D", size = px(24))
  ) |>
  tab_style(
    locations = cells_title("subtitle"),
    style = cell_text(font = "Lato", weight = 400, color = "#525252", size = px(14))
  ) |> 
  tab_style(
    locations = cells_column_spanners(),
    style = cell_text(weight = 600)
  ) |> 
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = cell_text(
      font = google_font("Lato"), 
      weight = 400,
      color = "#525252"
    )
  ) |> 
  tab_style(
    locations = cells_body(columns = c(fasteignagjold, haekkun_verd)),
    style = cell_borders(
      sides = "left", 
      weight = px(2) 
    )
  ) |> 
  opt_table_font(font = "Lato", weight = 400) |> 
  opt_vertical_padding(scale = 0.6) |> 
  tab_options(
    heading.align = "left",
    table.margin.left = px(0),
    table.margin.right = px(0),
    table.background.color = "#faf9f9",
    stub.background.color = "#faf9f9", 
    column_labels.background.color = "#faf9f9",
    column_labels.font.weight = "normal", 
    column_labels.border.top.style = "none", column_labels.border.bottom.width = px(2), 
    column_labels.border.bottom.color = "#525252",
    source_notes.font.size = 12,
    table.font.size = 14
  ) |> 
  gtsave(
    filename = here("greinar", "fasteignagjold", "Figures", "table.png"),
    expand = 4,
    zoom = 7,
    vwidth = 1100
  )

# theme_metill



# gt_theme_538

