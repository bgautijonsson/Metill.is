p <- plot_dat |> 
  ggplot(aes(dags, breyting_fjoldi)) +  
  geom_hline(yintercept = 0, lty = 2) +  
  geom_line(
    arrow = arrow(type = "closed", length = unit(0.2, "cm"))
  ) +  
  scale_x_date(
    breaks = breaks_width("year"),
    labels = label_date_short(),
    expand = expansion()
  ) +  
  scale_y_tufte(
    breaks = tufte_breaks(plot_dat$breyting_fjoldi),
    labels = label_number(big.mark = ".", decimal.mark = ","),
    limits = c(NA, NA)
  ) +  
  
  labs(
    x = NULL, 
    y = NULL,  
    title = "Almennur vinnumarkaður er stærri en nokkru sinni fyrr",
    subtitle = "Fjöldi starfandi borinn saman við sama mánuð árið 2018"
  ) +  
  theme(plot.margin = margin(t = 5, r = 20, b = 5, l = 5))


library(magick)
library(patchwork)

toy <- magick::image_read("toy.jpg") |> image_ggplot()

p_combined <- cowplot::plot_grid(
  toy, p, 
  ncol = 1,
  rel_heights = c(0.9, 1)
)

p_combined

ggsave(
  plot = p_combined,
  filename = "vinnumarkadur_bosi.png",
  width = 8, height = 1.2 * 8, scale = 1
)
