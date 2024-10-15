library(ggiraph)

plot_dat <- d |> 
  filter(
    tiundarbreyta == "Ráðstöfunartekjur",
    name %in% c("Ráðstöfunartekjur (Tekjur - Skattar)", "Fjöldi í hóp"),
  ) |> 
  pivot_wider() |> 
  rename(fjoldi = "Fjöldi í hóp") |> 
  pivot_longer(c(-ar, -tiundarbreyta, -tiundarhluti, -fjoldi)) |> 
  group_by(ar, name) |> 
  mutate(
    p = value / sum(value)
  ) |> 
  ungroup() |> 
  select(-tiundarbreyta) |> 
  mutate(
    value = (value / fjoldi),
    value = vnv_convert(value, ar)
  ) |> 
  mutate(
    index = value / value[ar == min(ar)],
    .by = tiundarhluti
  ) |> 
  mutate(
    text = str_c(
      "<b>Tekjutíund: ", tiundarhluti, "</b>", "\n",
      "Ár: ", ar, "\n",
      "Heildartekjur (á mann á mánuði): ",
      number(value * 1e6 / 12, suffix = " kr", big.mark = ".", decimal.mark = ","), "\n",
      "Breyting (síðan 1997): ", 
      ifelse(index >= 1, "+", ""),
      percent(index - 1, accuracy = 0.01, big.mark = ".", decimal.mark = ","
      )
    ),
    tiundarhluti = factor(tiundarhluti, levels = 10:1)
  )

p <- plot_dat |> 
  ggplot(aes(ar, index, col = tiundarhluti, group = tiundarhluti)) +
  geom_hline(yintercept = 1, lty = 2, linewidth = 0.3) +
  geom_line_interactive() +
  geom_point_interactive(size = 1) +
  geom_text_interactive(
    data = ~filter(.x, ar == max(ar)),
    aes(label = tiundarhluti),
    nudge_x = 1
  ) +
  scale_x_continuous(
    
  ) +
  scale_y_continuous(
    labels = \(x) percent(x - 1)
  ) +
  scale_colour_brewer(palette = "RdBu", direction = -1) +
  labs(
    x = NULL,
    y = NULL,
    col = "Tíundarhluti"
  )


girafe(
  ggobj = p
)
