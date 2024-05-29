library(tidyverse)
library(readxl)
library(purrr)
library(here)
library(ggh4x)
library(ggiraph)
library(patchwork)
library(metill)
theme_set(theme_metill())

make_fasteignamat_total <- function() {
  d <- here("greinar", "fasteignagjold", "data", "fasteignamat_total_clean.csv") |> 
    read_csv() |> 
    arrange(year) |> 
    mutate(
      sveitarfelag = fct_reorder(sveitarfelag, fasteignamat, .fun = max),
      svf_ordered1 = fct_reorder(sveitarfelag, fasteignamat, .fun = \(x) head(x, 1)),
      svf_ordered2 = fct_reorder(sveitarfelag, fasteignamat, .fun = \(x) tail(x, 1))
    ) 
  
  point_size <- 2
  segment_lw <- 0.1
  text_size <- 2.7
  
  p1 <- d |> 
    filter(
      year == 2018
    ) |> 
    ggplot(aes(fasteignamat, svf_ordered1)) +
    geom_text_interactive(
      aes(x = 0, label = str_c(sveitarfelag, " "), data_id = sveitarfelag),
      hjust = 1,
      size = text_size,
      hover_nearest = FALSE
    ) +
    geom_segment_interactive(
      aes(yend = svf_ordered1, xend = 0, data_id = sveitarfelag),
      lty = 2, 
      alpha = 0.5,
      linewidth = segment_lw,
      hover_nearest = FALSE
    ) +
    geom_point_interactive(
      aes(data_id = sveitarfelag),
      size = point_size,
      hover_nearest = FALSE
    ) +
    scale_x_continuous(
      expand = expansion(c(0.3, 0.05)),
      breaks = seq(0, 14, by = 2)/1000,
      limits = c(0, 0.0135),
      labels = label_hlutf(),
      guide = guide_axis_truncated(
        trunc_lower = 0
      )
    ) +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(t = 5, r = 25, b = 5, l = 5),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Árið 2018"
    )
  
  
  p2 <- d |> 
    filter(
      year == 2024
    ) |> 
    ggplot(aes(fasteignamat, svf_ordered2)) +
    geom_text_interactive(
      aes(x = 0, label = str_c(sveitarfelag, " "), data_id = sveitarfelag),
      hjust = 1,
      size = text_size,
      hover_nearest = FALSE
    ) +
    geom_segment_interactive(
      aes(yend = svf_ordered2, xend = 0, data_id = sveitarfelag),
      lty = 2, 
      alpha = 0.5,
      linewidth = segment_lw,
      hover_nearest = FALSE
    ) +
    geom_point_interactive(
      aes(data_id = sveitarfelag),
      size = point_size,
      hover_nearest = FALSE
    ) +
    scale_x_continuous(
      expand = expansion(c(0.3, 0.05)),
      breaks = seq(0, 14, by = 2)/1000,
      limits = c(0, 0.0135),
      labels = label_hlutf(),
      guide = guide_axis_truncated(
        trunc_lower = 0
      )
    ) +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(t = 5, r = 25, b = 5, l = 5),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Árið 2024"
    )
  
  
  p3 <- d |> 
    ggplot(aes(year, fasteignamat))
  
  n_subgroups <- 10
  for (i in 0:(n_subgroups - 1)) {
    p3 <- p3 + 
      geom_line_interactive(
        data = filter(d, (as.numeric(svf_ordered1) %% n_subgroups) == i),
        aes(group = sveitarfelag, data_id = sveitarfelag),
        alpha = 0.5,
        hover_nearest = FALSE
      )
  }
  
  p3 <- p3 +
    scale_x_continuous(
      guide = guide_axis_truncated()
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      # expand = expansion(c(0, 0.05)),
      breaks = breaks_extended(),
      labels = label_hlutf(accuray = 0.1),
      guide = guide_axis_truncated()
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Þróun"
    )
  
  
  p <- (p1 + p2) / p3 +
    plot_annotation(
      title = "Heildarhlutfall fasteignamats sem skilar sér í fasteignagjöld sveitarfélaga",
      subtitle = "Láttu músina yfir sveitarfélag til að einblína á það"
    ) +
    plot_layout(
      heights = c(1.1, 1)
    )
  
  girafe(
    ggobj = p,
    width_svg = 11,
    height_svg = 1.2 * 11,
    options = list(
      opts_tooltip(
        opacity = 0.8, 
        use_fill = TRUE,
        use_stroke = FALSE, 
        css = "padding:5pt;font-family: Open Sans;font-size:1rem;color:white"
      ),
      opts_hover(css = ""),
      opts_hover_inv(css = "opacity:0.05"), 
      opts_toolbar(saveaspng = TRUE),
      opts_zoom(max = 1)
    )
  )
}

make_fasteignamat_total()