d |> 
  filter(
    rekstrarform != "Alls starfandi",
    kyn == "Alls",
    bakgrunnur == "Alls"
  ) |> 
  count(dags, tegund, wt = starfandi) |> 
  mutate(
    man = month(dags)
  ) |> 
  mutate(
    id = row_number(),
    n_smoothed = mgcv::gam(n ~ s(id, bs = "ad") + s(man, bs = "cc"), method = "REML", family = gaussian()) |> predict(),
    .by = tegund
  ) |> 
  pivot_longer(c(n, n_smoothed), values_to = "n") |> 
  mutate(
    percent_change = n / lag(n),
    .by = c(man, tegund, name)
  ) |> 
  drop_na() |> 
  filter(
    dags >= clock::date_build(2000)
  ) |> 
  mutate(
    tegund = fct_relevel(tegund, "Opinbert") |> 
      fct_recode(
        "Hið opinbera" = "Opinbert",
        "Einkageiri" = "Annad"
      )
  ) |> 
  select(-n) |> 
  pivot_wider(names_from = name, values_from = percent_change) |> 
  ggplot(aes(dags, n)) +
  geom_hline(
    yintercept = 1,
    lty = 2,
    linewidth = 0.3
  ) +
  geom_line(
    aes(col = tegund, lty = tegund),
    linewidth = 0.3,
    alpha = 0.2
    ) +
  geom_line(
    aes(y = n_smoothed, col = tegund, lty = tegund)
    ) +
  # geom_point(
  #   data = ~filter(.x, dags >= clock::date_build(2024, 6)),
  #   aes(col = tegund, lty = tegund),
  #   size = 1
  # ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_y_continuous(
    breaks = breaks_extended(9),
    labels = \(x) percent(x - 1),
    trans = "log10",
    guide = ggh4x::guide_axis_truncated(),
    expand = expansion()
  ) +
  scale_colour_brewer(
    palette = "Set1"
  ) +
  scale_linetype_manual(
    values = c(1, 5)
  ) +
  # ggforce::facet_zoom(
  #   x = dags >= clock::date_build(2024, 6),
  #   zoom.size = 0.9
  # ) +
  coord_cartesian(ylim = c(0.85, 1.16)) +
  labs(
    title = "Árleg breyting í fjölda starfandi",
    subtitle = "Grunngögn sýnd með fölum lit | Þróun sýnd með skærum lit",
    col = NULL,
    lty = NULL,
    x = NULL,
    y = NULL
  ) +
  theme(
    legend.position = c(0.2, 0.85),
    legend.margin = margin()
    
  )


ggsave(
  filename = "greinar/vinnuafl/Figures/arleg_breyting.png",
  width = 8, height = 8, scale = 0.7
)
