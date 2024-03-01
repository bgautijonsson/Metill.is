plot_var <- "applicants_non_ukraine"
scaling_var <- "per_pop"
start_date <- clock::date_build(2022, 3, 1)
end_date <- clock::date_build(2023, 12, 1)
title <- "Hvar bíða flestir einstaklingar eftir niðurstöðu hælisumsóknar sinnar?"
subtitle <- "Sýnt sem fjöldi á 100.000 íbúa hvers lands"
number_labels <- label_number(big.mark = ".", decimal.mark = ",")
y_upper <- NA


plot_dat <- d |> 
  filter(name == plot_var) |> 
  arrange(time) |> 
  rename(
    dags = time, 
    flottafjoldi = value,
    per_pers = all_of(scaling_var)
  ) |>  
  # filter(dags <= end_date) |> 
  drop_na(per_pers) |> 
  select(dags, land, value = per_pers) |> 
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
    size = as_factor(linewidth)
  )

p3 <- plot_dat |> 
  ggplot(aes(dags, value)) +
  geom_line_interactive(
    data = plot_dat |> 
      filter(colour == litur_annad),
    aes(group = land, colour = litur_annad, data_id = land),
    alpha = 0.3,
    col = litur_annad
  ) +
  geom_line_interactive(
    data = plot_dat |> 
      filter(colour != litur_annad),
    aes(group = land, colour = colour, data_id = land),
    linewidth = 1
  ) +
  scale_x_date(
    breaks = unique(plot_dat$dags),
    limits = c(min(plot_dat$dags), max(plot_dat$dags) + days(25)),
    labels = label_date_short(),
    expand = expansion(add = 15),
    guide = guide_axis_truncated(
      trunc_lower = min(plot_dat$dags),
      trunc_upper = max(plot_dat$dags)
    )
  ) +
  scale_y_continuous(
    breaks = breaks_extended(6),
    labels = number_labels,
    limits = c(0, NA),
    expand = expansion(c(0, 0.01)),
    guide = guide_axis_truncated()
  ) +
  scale_colour_identity() +
  coord_cartesian(ylim = c(0, y_upper), clip = "on") +
  theme(
    plot.margin = margin(t = 5, r = 35, b = 5, l = 5)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = title,
    subtitle = subtitle,
    caption = caption
  )


p3


ggsave(
  plot = p3,
  filename = "greinar/flottafolk/Figures/cover.png",
  width = 8, height = 0.621 * 8, scale = 1.3
)


ggsave(
  plot = p3 + theme_metill(type = "blog"),
  filename = "greinar/flottafolk/Figures/cover_fp.png",
  width = 8, height = 0.621 * 8, scale = 1.3
)
