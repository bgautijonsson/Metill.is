make_plot1 <- function() {
  
  start_date <- clock::date_build(2010)
  end_date <- clock::date_build(2022)
  
  p1 <- d |>
    filter(dags == start_date) |> 
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
      size = as_factor(linewidth),
      land_ordered = glue("<i style='color:{colour}'>{land}</i>"),
      land_ordered = fct_reorder(land_ordered, perc_innfl)
    ) |> 
    ggplot(aes(perc_innfl, land_ordered, col = colour, size = size)) +
    geom_text_interactive(
      aes(x = 0, label = str_c(land, " "), data_id = land),
      hjust = 1,
      size = 4
    ) +
    geom_point_interactive(
      aes(data_id = land)
    ) +
    geom_segment_interactive(
      aes(yend = land_ordered, xend = 0, linewidth = linewidth, data_id = land),
      lty = 2, 
      alpha = 0.5
    ) +
    scale_x_continuous(
      expand = expansion(c(0, 0.05)),
      breaks = breaks_extended(6),
      limits = c(0, 0.3),
      labels = label_hlutf(),
      guide = guide_axis_truncated(
        trunc_lower = 0
      )
    ) +
    scale_colour_identity() +
    scale_size_manual(values = c(1.5, 3)) +
    scale_linewidth(
      range = c(0.2, 0.4)
    ) +
    coord_cartesian(clip = "off", xlim = c(-0.15, NA)) +
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
      subtitle = glue("Hlutfall í {month(start_date, label = T, abbr = F)} {year(start_date)}"),
      caption = caption
    )
  
  
  p2 <- d |> 
    filter(dags == end_date) |> 
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
      size = as_factor(linewidth),
      land_ordered = glue("<i style='color:{colour}'>{land}</i>"),
      land_ordered = fct_reorder(land_ordered, perc_innfl)
    ) |> 
    ggplot(aes(perc_innfl, land_ordered, col = colour, size = size)) +
    geom_text_interactive(
      aes(x = 0, label = str_c(land, " "), data_id = land),
      hjust = 1,
      size = 4
    ) +
    geom_point_interactive(
      aes(data_id = land)
    ) +
    geom_segment_interactive(
      aes(yend = land_ordered, xend = 0, linewidth = linewidth, data_id = land),
      lty = 2, 
      alpha = 0.5
    ) +
    scale_x_continuous(
      expand = expansion(c(0, 0.05)),
      breaks = breaks_extended(6),
      limits = c(0, NA),
      labels = label_hlutf(),
      guide = guide_axis_truncated(
        trunc_lower = 0
      )
    ) +
    scale_colour_identity() +
    scale_size_manual(values = c(1.5, 3)) +
    scale_linewidth(
      range = c(0.2, 0.4)
    ) +
    coord_cartesian(clip = "off", xlim = c(-0.15, NA)) +
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
      subtitle = glue("Hlutfall í {month(end_date, label = T, abbr = F)} {year(end_date)}"),
      caption = caption
    )
  
  plot_dat <- d |> 
    arrange(dags) |>  
    filter(dags >= start_date) |>
    select(dags, land, value = perc_innfl) |> 
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
  
  p3 <- plot_dat |> 
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
      breaks = breaks_extended(6),
      labels = label_hlutf(),
      limits = c(0, NA),
      expand = expansion(c(0, 0.01)),
      guide = guide_axis_truncated()
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
  
  p <- (
    (p1 + labs(title = NULL, caption = NULL)) + 
      (p2 + labs(title = NULL, caption = NULL))
  ) / 
    p3 +
    plot_annotation(
      subtitle = str_c(
        "Láttu músina yfir land til að einblína á það"
      ),
      caption = caption
    )
  
  girafe(
    ggobj = p,
    width_svg = 11,
    height_svg = 1.1 * 11,
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

make_plot1()