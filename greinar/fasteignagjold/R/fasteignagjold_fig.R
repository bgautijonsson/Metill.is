library(tidyverse)
library(readxl)
library(metill)
theme_set(theme_metill())
library(patchwork)
library(geomtextpath)
library(ggtext)
library(here)

fasteignamat <- read_excel(
  here("data-raw", "fasteignagjold.xlsx"),
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
  select(sveitarfelag, fasteignamat)

read_excel(
  here("greinar", "fasteignagjold", "data", "Sveitarfélög eftir tegundum eigna.xlsx")
) |>
  janitor::clean_names() |>
  filter(
    tegund_eigna == "Íbúðareignir"
  ) |> 
  mutate(
    avg = fasteignamat_2024 / fjoldi
  )

d <- read_excel(
  here("greinar", "fasteignagjold", "data", "Sveitarfélög eftir tegundum eigna.xlsx")
) |>
  janitor::clean_names() |>
  filter(
    tegund_eigna == "Íbúðareignir"
  ) |>
  mutate(
    haekkun_verd = (fasteignamat_2024 - fasteignamat_2023) / fjoldi
  ) |>
  select(
    sveitarfelag, haekkun_verd
  ) |>
  inner_join(
    fasteignamat
  ) |>
  mutate(
    haekkun_mat = haekkun_verd * fasteignamat
  ) |>
  select(sveitarfelag, fasteignamat, haekkun_verd, haekkun_mat)



p1 <- d |>
  mutate(
    color = if_else(
      str_detect(sveitarfelag, "Seltjarnarnes"),
      "#a50f15",
      "black"
    ),
    sveitarfelag = fct_reorder(sveitarfelag, haekkun_verd) |>
      fct_recode(
        "<b style='color:#a50f15;'>Seltjarnarnesbær</b>" = "Seltjarnarnesbær",
        "Grundarfjarðarbær<span style='color:#faf9f9;'>__</span>" = "Grundarfjarðarbær"
      )
  ) |>
  ggplot(aes(haekkun_verd, sveitarfelag)) +
  geom_segment(
    aes(xend = 0, yend = sveitarfelag, color = color),
    linewidth = 0.5,
    alpha = 0.5
  ) +
  geom_point(
    aes(color = color),
    size = 3
  ) +
  geom_textcurve(
    data = tibble(
      x = 10e6, xend = 16.6e6,
      y = 34, yend = 50.1,
      label = str_c(
        "Fasteignamat hækkaði að meðaltali\n",
        "um 16,6 milljónir á Seltjarnarnesi"
      )
    ),
    aes(x = x, xend = xend, y = y, yend = yend, label = label),
    arrow = arrow(length = unit(0.15, "inches"), type = "closed"),
    angle = 90,
    curvature = 0.2,
    col = "#a50f15",
    vjust = -0.25,
    halign = "right"
  ) +
  scale_x_continuous(
    labels = label_isk(scale = 1e-6),
    # limits = c(0, 90000),
    expand = expansion()
  ) +
  scale_color_identity() +
  coord_cartesian(clip = "off", xlim = c(0, 17e6)) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Meðalhækkun fasteignamats"
  ) +
  theme(
    axis.text.y = element_markdown(),
    axis.ticks.y = element_blank()
  )

p2 <- d |>
  mutate(
    color = if_else(
      str_detect(sveitarfelag, "Seltjarnarnes"),
      "#a50f15",
      "black"
    ),
    sveitarfelag = fct_reorder(sveitarfelag, fasteignamat) |>
      fct_recode(
        "<b style='color:#a50f15;'>Seltjarnarnesbær</b>" = "Seltjarnarnesbær"
      )
  ) |>
  ggplot(aes(fasteignamat, sveitarfelag)) +
  geom_segment(
    aes(xend = 0, yend = sveitarfelag, color = color),
    linewidth = 0.5,
    alpha = 0.5
  ) +
  geom_point(
    aes(color = color),
    size = 3
  ) +
  geom_textcurve(
    data = tibble(
      x = 0.011, xend = 0.0043,
      y = 16, yend = 11,
      label = str_c(
        "en lágt hlutfall fasteignamats í\nfasteignagjöldum"
      )
    ),
    aes(x = x, xend = xend, y = y, yend = yend, label = label),
    arrow = arrow(length = unit(0.15, "inches"), type = "closed"),
    angle = 90,
    curvature = -0.3,
    col = "#a50f15",
    vjust = -0.3,
    halign = "left"
  ) +
  scale_x_continuous(
    labels = label_hlutf(),
    # limits = c(0, 90000),
    expand = expansion()
  ) +
  scale_color_identity() +
  coord_cartesian(clip = "off", xlim = c(0, NA)) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "% af fasteignamati sem fer í útreikning fasteignagjalda"
  ) +
  theme(
    axis.text.y = element_markdown(),
    axis.ticks.y = element_blank()
  )

p3 <- d |>
  mutate(
    color = if_else(
      str_detect(sveitarfelag, "Seltjarnarnes"),
      "#a50f15",
      "black"
    ),
    sveitarfelag = fct_reorder(sveitarfelag, haekkun_mat) |>
      fct_recode(
        "<b style='color:#a50f15;'>Seltjarnarnesbær</b>" = "Seltjarnarnesbær",
        "Grundarfjarðarbær<span style='color:#faf9f9;'> _____</span>" = "Grundarfjarðarbær"
      )
  ) |>
  ggplot(aes(haekkun_mat, sveitarfelag)) +
  geom_segment(
    aes(xend = 0, yend = sveitarfelag, color = color),
    linewidth = 0.5,
    alpha = 0.5
  ) +
  geom_point(
    aes(color = color),
    size = 3
  ) +
  scale_x_continuous(
    labels = label_isk(scale = 1e-3),
    # limits = c(0, 90000),
    expand = expansion()
  ) +
  scale_color_identity() +
  geom_textcurve(
    data = tibble(
      x = 80000, xend = 66400,
      y = 33, yend = 45.8,
      label = str_c(
        "dregur úr aukningu gjaldanna\nvegna hækkandi mats"
      )
    ),
    aes(x = x, xend = xend, y = y, yend = yend, label = label),
    arrow = arrow(length = unit(0.15, "inches"), type = "closed"),
    angle = 90,
    curvature = 0.2,
    col = "#a50f15",
    vjust = -0.3,
    halign = "left"
  ) +
  coord_cartesian(clip = "off", xlim = c(0, 90000)) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Áhrif fasteignamats á hækkun árlegra fasteigna-, vatns- og fráveitugjalda sveitarfélaga"
  ) +
  theme(
    axis.text.y = element_markdown(),
    axis.ticks.y = element_blank()
  )


p <- (p1 + p2) / p3 +
  plot_annotation(
    title = "Hækkandi fasteignamat hefur mismunandi áhrif á fasteignagjöld eftir sveitarfélagi",
    subtitle = str_c(
      "Fasteignagjöld samanstanda af nokkrum liðum: fasteignaskatti, lóðarleigu, og vatns- sorp- og fráveitugjaldi. ",
      "Þessir liðir geta ýmist vera reiknaðir út frá stærð\nfasteignar, fasteignamati hennar, föstum krónufjölda eða blöndu af þessu öllu."
    ),
    caption = str_c(
      "Fasteignagjöld: https://www.samband.is/verkefnin/fjarmal/tekjustofnar-sveitarfelaga/fasteignaskattur/",
      "\n",
      "Fasteignamat: https://fasteignaskra.is/fasteignir/fasteignamat/2024/",
      "\n",
      "Kóði: https://github.com/bgautijonsson/sunnudagurtilsveitarfelaga"
    )
  )


ggsave(
  plot = p,
  filename = here("Figures", "fasteignagjold.png"),
  width = 8, height = 1.2 * 8, scale = 1.7
)
