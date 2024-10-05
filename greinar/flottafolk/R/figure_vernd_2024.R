library(tidyverse)
library(scales)
library(visitalaneysluverds)
library(metill)
library(patchwork)
library(glue)
library(eurostat)
library(ggh4x)
library(ggiraph)
library(gt)
library(gtExtras)
library(ggtext)
library(here)

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

theme_set(theme_metill())


caption <- "Mynd eftir @bggjonsson hjá metill.is byggð á gögnum Eurostat um fólksflutninga: https://metill.is/greinar/flottafolk\nGögn og kóði: https://github.com/bgautijonsson/Metill.is/tree/master/greinar/flottafolk"

litur_island <- "#08306b"
litur_danmork <- "#e41a1c"
litur_finnland <- "#3690c0"
litur_noregur <- "#7f0000"
litur_svithjod <- "#fd8d3c"
litur_luxemborg <- "black"
litur_total <- "#005824"
litur_annad <- "#737373"

d <- here("greinar", "flottafolk", "data", "raw_data.csv") |>
  read_csv()

data_hist <- here("greinar", "flottafolk", "data", "data_hist.csv") |>
  read_csv()


d |>
  filter(
    year(time) >= 2024,
    month(time) <= 6,
    name %in% c("grants", "asylum_applicants_non_ukraine")
  ) |>
  filter(is.na(value)) |>
  distinct(land)

plot_dat <- d |>
  filter(
    year(time) >= 2024,
    month(time) <= 6,
    land != "Spánn",
    name %in% c("grants", "asylum_applicants_non_ukraine")
  ) |>
  summarise(
    value = sum(value, na.rm = TRUE),
    pop = max(pop, na.rm = TRUE),
    value = value / pop * 100000,
    .by = c(land, name)
  ) |>
  select(-pop) |>
  pivot_wider() |>
  left_join(
    data_hist |>
      filter(year(time) == 2023) |>
      select(land, colour, linewidth, size)
  ) |>
  mutate(
    total = grants + asylum_applicants_non_ukraine
  )



p1 <- plot_dat |>
  mutate(
    value = total,
    land = fct_reorder(land, value),
    land_ordered = glue("<i style='color:{colour}'>{land}</i>"),
    land_ordered = fct_reorder(land_ordered, value)
  ) |>
  ggplot(aes(value, land_ordered, col = colour, linewidth = linewidth, size = size)) +
  geom_segment(aes(xend = 0, yend = land_ordered)) +
  geom_point() +
  scale_x_continuous(
    expand = expansion(c(0, 0.12)),
    breaks = breaks_extended(6),
    limits = c(0, NA),
    guide = guide_axis_truncated(
      trunc_lower = 0
    )
  ) +
  scale_colour_identity() +
  scale_size(range = c(1.5, 3)) +
  scale_linewidth(
    range = c(0.2, 0.4)
  ) +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.text.y = element_markdown(),
    axis.ticks.y = element_blank(),
    plot.subtitle = element_markdown()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "<b style='font-size:15pt;'>Umsóknir um vernd/hæli</b><br><i style='font-size:13pt;'>Samtals</i>"
  )


p2 <- plot_dat |>
  mutate(
    value = grants,
    land = fct_reorder(land, value),
    land_ordered = glue("<i style='color:{colour}'>{land}</i>"),
    land_ordered = fct_reorder(land_ordered, value)
  ) |>
  ggplot(aes(value, land_ordered, col = colour, linewidth = linewidth, size = size)) +
  geom_segment(aes(xend = 0, yend = land_ordered)) +
  geom_point() +
  scale_x_continuous(
    expand = expansion(c(0, 0.12)),
    breaks = breaks_extended(6),
    limits = c(0, NA),
    guide = guide_axis_truncated(
      trunc_lower = 0
    )
  ) +
  scale_colour_identity() +
  scale_size(range = c(1.5, 3)) +
  scale_linewidth(
    range = c(0.2, 0.4)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.text.y = element_markdown(),
    axis.ticks.y = element_blank(),
    plot.subtitle = element_markdown()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "<br><i style='font-size:13pt;'>Flóttafólk frá Úkraínu</i>"
  )


p3 <- plot_dat |>
  mutate(
    value = asylum_applicants_non_ukraine,
    land = fct_reorder(land, value),
    land_ordered = glue("<i style='color:{colour}'>{land}</i>"),
    land_ordered = fct_reorder(land_ordered, value)
  ) |>
  ggplot(aes(value, land_ordered, col = colour, linewidth = linewidth, size = size)) +
  geom_segment(aes(xend = 0, yend = land_ordered)) +
  geom_point() +
  scale_x_continuous(
    expand = expansion(c(0, 0.12)),
    breaks = breaks_extended(6),
    limits = c(0, NA),
    guide = guide_axis_truncated(
      trunc_lower = 0
    )
  ) +
  scale_colour_identity() +
  scale_size(range = c(1.5, 3)) +
  scale_linewidth(
    range = c(0.2, 0.4)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.text.y = element_markdown(),
    axis.ticks.y = element_blank(),
    plot.subtitle = element_markdown()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "<br><i style='font-size:13pt;'>Flóttafólk frá öðrum löndum</i>"
  )



p <- p1 + p2 + p3 +
  plot_annotation(
    title = "Umsóknir um vernd í Evrópulöndum (2024 janúar - júní)",
    subtitle = "Sýnt sem fjöldi á 100.000 íbúa móttökulands",
    caption = caption
  )

ggsave(
  plot = p,
  filename = here("greinar", "flottafolk", "figures", "figure_vernd_2024.png"),
  width = 8, height = 0.4 * 8, scale = 1.8
)
