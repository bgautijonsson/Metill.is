library(tidyverse)
library(geomtextpath)
library(glue)
library(metill)
library(ggh4x)
library(here)
theme_set(theme_metill())

p <- crossing(
  fmverd = 100000 * seq(1, 10),
  fm = c(80, 120, 160)
) |> 
  mutate(
    fasteignamat = fmverd * fm,
    Kópavogsbær = 0.00282 * fasteignamat,
    Reykjavíkurborg = 18322 + 798.31 * fm + 0.0018 * fasteignamat,
    Garðabær = 0.00315 * fasteignamat,
    Seltjarnarnesbær = 0.00394 * fasteignamat,
    fm2 = glue("{fm}~m^2") |> 
      fct_reorder(fm)
  ) |> 
  pivot_longer(c(Kópavogsbær, Reykjavíkurborg, Garðabær, Seltjarnarnesbær)) |> 
  ggplot(aes(fmverd, value)) +
  geom_line(
    aes(group = name, lty = name, col = name),
    linewidth = 0.8
  ) +
  scale_x_continuous(
    labels = label_isk(scale = 1e-3),
    guide = guide_axis_truncated(),
    limits = c(0, NA)
  ) +
  scale_y_continuous(
    breaks = breaks_width(100000),
    labels = label_isk(scale = 1e-3),
    guide = guide_axis_truncated(), 
    position = "right",
    limits = c(0, NA)
  ) +
  scale_colour_brewer(
    palette = "Set1",
    guide = guide_legend(override.aes = list(linewidth = 1))
  ) +
  facet_wrap("fm2", labeller = label_parsed, ncol = 1) +
  theme(
    legend.position = c(0.29, 0.93),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 15),
    legend.key.width = unit(1.1, "cm")
  ) +
  labs(
    x = "Fermetraverð",
    y = "Fasteignagjöld",
    col = NULL,
    lty = NULL
  )

p

ggsave(
  plot = p,
  filename = here("greinar", "fasteignagjold", "figures", "compare_fasteignagjold_rvk_kop.png"),
  width = 0.4 * 8, height = 8, scale = 1.3
)
