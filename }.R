p <- d |> 
  mutate(
    tegund = str_to_title(tegund),
    label = ifelse((eining == 10) & (aldur == "16 - 24 ára") & (tegund == "Atvinnutekjur"),
                   "Breyting meðaltekna aldurshóps", 
                   NA_character_)
  ) |> 
  filter(eining != 95) |> 
  ggplot(aes(eining, breyting + 1, ids = eining, group = eining)) + 
  geom_hline(yintercept = 1, lty = 2, alpha = 0.4, linewidth = 0.4) +
  geom_hline(
    aes(yintercept = medaltal_breyting + 1),
    alpha = 0.5,
    lty = 3,
    colour = "#e41a1c"
  ) +
  geom_text(
    aes(
      x = 45, 
      y = (medaltal_breyting + 1) * 1.15,
      label = label
    ),
    hjust = 0, vjust = 1,
    colour = "#e41a1c"
  ) +
  geom_point() +
  geom_segment(aes(xend = eining, yend = 1), lty = 2, alpha = 0.5) +
  scale_x_continuous(
    breaks = c(seq(10, 90, by = 10), 99),
    labels = label_number(suffix = "%")
  ) +
  scale_y_continuous(
    labels = function(x) hlutf(x - 1),
    trans = "log10"
  ) +
  facet_grid(cols = vars(aldur), rows = vars(tegund)) +
  labs(
    x = "Tíundamörk",
    y = "Hlutfallsleg breyting frá árinu 1990",
    title = "Hvernig hafa atvinnu- og heildartekjur eftir aldurs- og tekjuhópum breyst síðan 1990? (Fast verðlag 2023)",
    subtitle = "Ár: {next_state}"
  ) +
  transition_states(
    ar, 
    transition_length = 100, 
    state_length = 0,
    wrap = FALSE
  ) +
  ease_aes("cubic-in-out")


p_anim <- animate(
  p, 
  duration = 10,
  fps = 20,
  renderer = gifski_renderer(),
  width = 1200, 
  height = 0.5 * 1200,
  res = 75
)

anim_save(
  animation = p_anim,
  filename = "kaupmattur_anim.gif"
)

p_anim <- animate(
  p, 
  duration = 10,
  fps = 20,
  renderer = ffmpeg_renderer(format = "mp4"),
  width = 1200, 
  height = 746,
  res = 110
)

anim_save(
  animation = p_anim,
  filename = "Figures/kaupmattur_anim.mp4"
)



