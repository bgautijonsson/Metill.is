make_plot4 <- function() {
  
  plot_dat <- d |> 
    arrange(dags) |>  
    mutate(
      p = samtals / samtals[dags == min(dags)],
      .by = land
    ) |> 
    select(dags, land, value = p) |> 
    mutate(
      colour = case_when(
        land == "Ísland" ~ litur_island,
        land == "Danmörk" ~ litur_danmork,
        land == "Finnland" ~ litur_finnland,
        land == "Noregur" ~ litur_noregur,
        land == "Svíþjóð" ~ litur_svithjod,
        land == "Meðaltal" ~ litur_total,
        TRUE ~ litur_annad
      ),
      linewidth = 1 * (land == "Ísland"),
      size = as_factor(linewidth)
    )
  
  
  p1 <- plot_dat |> 
    filter(dags == max(dags)) |> 
    mutate(
      land_ordered = glue("<i style='color:{colour}'>{land}</i>"),
      land_ordered = fct_reorder(land_ordered, value)
    ) |> 
    ggplot(aes(value, land_ordered, col = colour, size = size)) +
    geom_vline(xintercept = 1, lty = 1, alpha = 0.3, linewidth = 0.3) +
    geom_text_interactive(
      aes(x = 0.9, label = str_c(land, " "), data_id = land),
      hjust = 1,
      size = 4
    ) +
    geom_point_interactive(
      aes(data_id = land)
    ) +
    geom_segment_interactive(
      aes(yend = land_ordered, xend = 1, linewidth = linewidth, data_id = land),
      lty = 2, 
      alpha = 0.5
    ) +
    scale_x_continuous(
      # expand = expansion(c(0.05, 0.05)),
      breaks = seq(0.9, 1.25, by = 0.05),
      limits = c(NA, 1.27),
      labels = \(x) hlutf(x - 1),
      guide = guide_axis_truncated(
        trunc_lower = 0.9,
        trunc_upper = 1.25
      )
    ) +
    scale_colour_identity() +
    scale_size_manual(values = c(1.5, 3)) +
    scale_linewidth(
      range = c(0.2, 0.4)
    ) +
    coord_cartesian(clip = "off", xlim = c(0.85, NA)) +
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
      subtitle = glue("Staðan í {month(max(plot_dat$dags), label = T, abbr = F)} {year(max(plot_dat$dags))}")
    )
  
  p2 <- plot_dat |> 
    ggplot(aes(dags, value)) +
    geom_line_interactive(
      data = plot_dat |> 
        filter(colour == litur_annad),
      aes(group = land, colour = litur_annad, data_id = land),
      alpha = 0.3,
      col = litur_annad
    ) +
    geom_line_interactive(
      data = plot_dat |> 
        filter(colour != litur_annad),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 1
    ) +
    scale_x_date(
      breaks = unique(plot_dat$dags),
      limits = c(min(plot_dat$dags), max(plot_dat$dags) + days(25)),
      labels = label_date_short(),
      expand = expansion(add = 15),
      guide = guide_axis_truncated(
        trunc_lower = min(plot_dat$dags),
        trunc_upper = max(plot_dat$dags)
      )
    ) +
    scale_y_continuous(
      breaks = breaks_extended(7),
      labels = \(x) hlutf(x - 1),
      limits = c(NA, NA),
      # expand = expansion(c(0, 0.01)),
      guide = guide_axis_truncated(
        trunc_upper = 1.25
      ),
      trans = "log10"
    ) +
    scale_colour_identity() +
    coord_cartesian(clip = "on") +
    theme(
      plot.margin = margin(t = 5, r = 35, b = 5, l = 5)
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Þróun"
    )
  
  
  p <- p1 + p2 +
    plot_annotation(
      title = "Breyting í fólksfjölda Evrópulanda",
      subtitle = str_c(
        glue("% breyting frá {year(min(plot_dat$dags))} til  {year(max(plot_dat$dags))} | "),
        "Láttu músina yfir land til að einblína á það"
      ),
      caption = caption
    )
  
  girafe(
    ggobj = p,
    width_svg = 11,
    height_svg = 0.621 * 11,
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
