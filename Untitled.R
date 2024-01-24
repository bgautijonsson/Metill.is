d |> 
  filter(
    name == plot_var
  ) |> 
  rename(
    dags = time, 
    flottafjoldi = value,
    per_pers = scaling_var
  ) |> 
  filter(dags == min(dags)) |> 
  drop_na(flottafjoldi) |> 
  mutate(
    colour = case_when(
      land == "Ísland" ~ litur_island,
      land == "Danmörk" ~ litur_danmork,
      land == "Finnland" ~ litur_finnland,
      land == "Noregur" ~ litur_noregur,
      land == "Svíþjóð" ~ litur_svithjod,
      TRUE ~ litur_annad
    ),
    linewidth = 1 * (land == "Ísland"),
    size = as_factor(linewidth),
    land_ordered = glue("<i style='color:{colour}'>{land}</i>"),
    land_ordered = fct_reorder(land_ordered, per_pers)
  ) |> 
  ggplot(aes(per_pers, land_ordered, col = colour, size = size)) +
  geom_text_interactive(
    aes(x = 0, label = str_c(land, " "), data_id = land),
    hjust = 1,
    size = 4
  ) +
  geom_point_interactive(
    aes(data_id = land)
  ) +
  geom_segment_interactive(
    aes(yend = land_ordered, xend = 0, linewidth = linewidth, data_id = land),
    lty = 2, 
    alpha = 0.5
  ) +
  scale_x_continuous(
    expand = expansion(c(0.15, 0.05)),
    breaks = breaks_extended(),
    limits = c(0, NA),
    labels = number_labels,
    guide = guide_axis_truncated(
      trunc_lower = 0
    )
  ) +
  scale_colour_identity() +
  scale_size_manual(values = c(2, 3)) +
  scale_linewidth(
    range = c(0.2, 0.4)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(t = 5, r = 25, b = 5, l = 5),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = glue("Fjöldi í {month(start_date, label = T, abbr = F)} {year(start_date)}"),
    caption = caption
  )
