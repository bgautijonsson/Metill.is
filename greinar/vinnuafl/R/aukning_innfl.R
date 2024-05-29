p <- d |> 
  filter(
    bakgrunnur == "Innflytjendur",
    rekstrarform == "Alls starfandi"
  ) |> 
  mutate(
    breyting = c(diff(starfandi), 0),
    m3 = slider::slide_dbl(breyting, sum, .before = 4)
  ) |> 
  mutate(
    man = month(dags, label = TRUE, abbr = FALSE, locale = "is_IS"),
    ar = year(dags)
  ) |> 
  filter(
    ar >= 2014
  ) |> 
  ggplot(aes(man, m3, group = ar, col = ar)) +
  geom_hline(
    yintercept = 0, 
    lty = 1, 
    alpha = 0.3, 
    linewidth = 0.2
  ) +
  geom_hline(
    yintercept = 3000, 
    lty = 2, 
    alpha = 0.4
    ) +
  geom_vline(
    xintercept = "maí",
    lty = 2,
    alpha = 0.2,
    linewidth = 0.2
  ) +
  geomtextpath::geom_textline(
    aes(label = ar, hjust = factor(ar))
    ) +
  scale_x_discrete(
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    breaks = c(3000, 1e3 * seq(-8, 8, by = 2)),
    guide = guide_axis_truncated(),
    limits = 1e3 * c(-8, 8)
  ) +
  scale_colour_distiller(
    breaks = seq(2016, 2024, by = 1),
    guide = guide_colorsteps(
      keyheight = unit(7, "cm")
    ),
    palette = "Greys",
    direction = 1,
    limits = c(2012, 2024)
  ) +
  scale_hjust_manual(
    values = c(0.4, 0.27, 0.4, 0.65, 0.55, 0.4, 0.5, 0.7, 0.57, 0.5, 0.8)
  ) +
  theme(legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    title = "Fimm mánaða breyting í fjölda starfandi með erlent ríkisfang",
    subtitle = "Fjöldi starfandi með erlent ríkisfang er hærri á sumrin og lægri á veturna",
    col = NULL,
    caption = "Reiknað út frá gögnum Hagstofu um starfandi samkvæmt skrám:\nhttps://px.hagstofa.is/pxis/pxweb/is/Samfelag/Samfelag__vinnumarkadur__vinnuaflskraargogn/VIN10052.px"
  )

 p

ggsave(
  plot = p,
  filename = "greinar/vinnuafl/figures/aukning_innfl.png",
  width = 8, height = 0.621 * 8, scale = 1.3
)

