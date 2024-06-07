p1 <- data_hist |> 
  filter(year(time) >= 2022) |> 
  summarise(
    value = sum(value),
    per_pop = sum(per_pop),
    .by = c(land, name, colour, linewidth, size)
  ) |> 
  pivot_longer(c(value, per_pop), names_to = "type") |> 
  pivot_wider() |> 
  mutate(
    ukraine = total - total_non_ukr,
    applicants_ukraine = asylum_applicants + ukraine,
    hlutf_ukraine = total / (asylum_applicants + ukraine),
    hlutf = total_non_ukr / asylum_applicants
  ) |> 
  filter(type == "per_pop") |> 
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
    subtitle = "<b style='font-size:15pt;'>Verndarveitingar</b><br><i style='font-size:13pt;'>Samtals</i>"
  )


p2 <- data_hist |> 
  filter(year(time) >= 2022) |> 
  summarise(
    value = sum(value),
    per_pop = sum(per_pop),
    .by = c(land, name, colour, linewidth, size)
  ) |> 
  pivot_longer(c(value, per_pop), names_to = "type") |> 
  pivot_wider() |> 
  mutate(
    ukraine = total - total_non_ukr,
    applicants_ukraine = asylum_applicants + ukraine,
    hlutf_ukraine = total / (asylum_applicants + ukraine),
    hlutf = total_non_ukr / asylum_applicants
  ) |> 
  filter(type == "per_pop") |> 
  mutate(
    value = total_non_ukr,
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
    subtitle = "<br><i style='font-size:13pt;'>Án flóttafólks frá Úkraínu</i>"
  )






p3 <- data_hist |> 
  filter(year(time) >= 2022) |> 
  summarise(
    value = sum(value),
    per_pop = sum(per_pop),
    .by = c(land, name, colour, linewidth, size)
  ) |> 
  pivot_longer(c(value, per_pop), names_to = "type") |> 
  pivot_wider() |> 
  mutate(
    ukraine = total - total_non_ukr,
    applicants_ukraine = asylum_applicants + ukraine,
    hlutf_ukraine = total / (asylum_applicants + ukraine),
    hlutf = total_non_ukr / asylum_applicants
  ) |> 
  filter(type == "per_pop") |> 
  mutate(
    value = applicants_ukraine,
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
    subtitle = "<b style='font-size:15pt;'>Umsóknir</b><br><i style='font-size:13pt;'>Samtals</i>"
  )


p4 <- data_hist |> 
  filter(year(time) >= 2022) |> 
  summarise(
    value = sum(value),
    per_pop = sum(per_pop),
    .by = c(land, name, colour, linewidth, size)
  ) |> 
  pivot_longer(c(value, per_pop), names_to = "type") |> 
  pivot_wider() |> 
  mutate(
    ukraine = total - total_non_ukr,
    applicants_ukraine = asylum_applicants + ukraine,
    hlutf_ukraine = total / (asylum_applicants + ukraine),
    hlutf = total_non_ukr / asylum_applicants
  ) |> 
  filter(type == "per_pop") |> 
  mutate(
    value = asylum_applicants,
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
    subtitle = "<br><i style='font-size:13pt;'>Án flóttafólks frá Úkraínu</i>"
  )


p <- p1 + p2 + p3 + p4 +
  plot_annotation(
    title = "Verndarkerfin í Evrópulöndum (2022 - 2023)",
    subtitle = "Sýnt sem fjöldi á 100.000 íbúa móttökulands",
    caption = caption
  )

ggsave(
  plot = p,
  filename = "Greinar/flottafolk/Figures/figure_vernd_2022-2023.png",
  width = 8, height = 1 * 8, scale = 1.3
)
