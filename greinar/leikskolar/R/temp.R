library(ggh4x)

d |> 
  filter(
    aldur == "1 árs"
  ) |> 
  mutate(
    p = n_leik / n_heild,
    p = pmin(p, 1)
  ) |> 
  filter(
    sveitarfelag %in% c(
      "Reykjavíkurborg",
      "Garðabær",
      "Kópavogsbær",
      "Mosfellsbær",
      "Hafnarfjarðarkaupstaður",
      "Seltjarnarnesbær"
    )
  ) |> 
  ggplot(aes(ar, p)) +
  geom_line(
    data = ~rename(.x, svf = sveitarfelag),
    aes(group = svf),
    col = "grey40",
    alpha = 0.3,
    linewidth = 0.3
  ) +
  geom_line(
    linewidth = 1.3
  ) +
  scale_x_continuous(
    expand = expansion(c(0, 0.1)),
    guide = guide_axis_truncated(),
    breaks = seq(1998, 2022, by = 4)
  ) +
  scale_y_continuous(
    guide = guide_axis_truncated(),
    labels = label_hlutf()
  ) +
  facet_wrap("sveitarfelag") +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL
  )
