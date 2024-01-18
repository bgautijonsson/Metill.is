library(ggh4x)
library(patchwork)


d2 <- read_csv2("greinar/landspitali/data/landspitali_erlend.csv") |> 
  pivot_longer(c(-ar)) |> 
  mutate(
    name = as.numeric(name),
    date = clock::date_build(ar, name),
    x = as.numeric(as.factor(date))
  )



d2 <- d2 |> 
  mutate(
    diff = value / value[ar == 2019],
    .by = name
  )





p1 <- d |> 
  mutate(
    lengd_legu_bmt_ambulant = lengd_legu_bmt_ambulant / 10
  ) |> 
  select(
    ar, 
    "Fjöldi legudaga á BMT\n(dvalir lengri en 24klst)" = bmt_legudagar, 
    "Meðallengd lega á BMT í klst\n(sem leiddu ekki til innlagnar)" = lengd_legu_bmt_ambulant
  ) |> 
  pivot_longer(c(-ar)) |> 
  filter(ar >= 2015) |> 
  ggplot(aes(ar, value)) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  scale_x_continuous(
    guide = guide_axis_truncated(),
    breaks = 2015:2022
  ) +
  scale_y_continuous(
    breaks = breaks_extended(4),
    expand = expansion(mult = 0.3),
    guide = guide_axis_truncated()
  ) +
  facet_wrap("name", scales = "free_y", ncol = 1) +
  labs(
    x = NULL,
    y = NULL,
    title = "Álag á bráðamóttöku Landspítala hefur aukist stöðugt frá 2015",
    subtitle = "Það tekur lengri tíma að útskrifa sjúklinga og æ fleiri liggja á bráðamóttöku lengur en sólarhring",
    caption = "Starfsemisupplýsingar LSH: https://www.landspitali.is/um-landspitala/spitalinn-i-tolum/starfsemisupplysingar-lsh/"
  )


ggsave(
  plot = p1,
  filename = "alag_bmt.png",
  width = 8, height = 1.1 * 8, scale = 1.1
)


p2 <- d2 |> 
  filter(ar >= 2020) |> 
  ggplot(aes(date, diff)) +
  geom_hline(
    yintercept = 1,
    lty = 2,
    alpha = 0.5
  ) +
  stat_smooth(
    geom = "line",
    linewidth = 1
  ) +
  stat_smooth(
    geom = "area",
    alpha = 0.2
  ) +
  scale_x_date(
    guide = guide_axis_truncated(trunc_lower = clock::date_build(2020)),
    limits = c(clock::date_build(2020, 1), clock::date_build(2023, 12)),
    breaks = breaks_width("1 year"),
    labels = label_date("%Y")
  ) +
  scale_y_continuous(
    guide = guide_axis_truncated(),
    breaks = c(0.2, 0.5, 1, 1.5, 2, 6),
    labels = function(x) hlutf(x - 1),
    trans = "log10",
    limits = c(NA, 6)
  ) +
  labs(
    title = "Mánaðarlegar innlagnir ósjúkratryggðra erlendra einstaklinga",
    subtitle = str_c(
      "Fjöldi sýndur miðað við sama mánuð árið 2019. ",
      "Mynd er á lograkvarða þ.a. -80% og +500% eru jafnlangt frá +0%"
    ),
    x = NULL,
    y = NULL
  )


p2

ggsave(
  plot = p2,
  filename = "erlendir.png",
  width = 8, height = 0.621 * 8, scale = 1.3
)



