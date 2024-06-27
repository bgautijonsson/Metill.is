library(ggplot2)
library(magick)
library(patchwork)

toy <- magick::image_read("toy.jpg") |> image_ggplot()
p <- magick::image_read("figures/almennur_fjoldi_methaedir.png") |> image_ggplot()

p_combined <- cowplot::plot_grid(
  toy, p, 
  ncol = 1,
  rel_heights = c(0.9, 1)
)

p_combined

ggsave(
  plot = p_combined,
  filename = "Figures/vinnumarkadur_bosi.png",
  width = 8, height = 1.2 * 8, scale = 1
)
