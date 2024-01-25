make_samantekt <- function(
    height = 11,
    width = 11,
    end_date = clock::date_build(2023, c(10, 7, 10)),
    number_labels = label_number(big.mark = ".", decimal.mark = ","),
    y_upper = NA
) {
  
  p11 <- d |> 
    filter(name == "asylum_applicants_non_ukraine") |> 
    rename(
      dags = time, 
      flottafjoldi = value,
      per_pers = per_pop
    ) |> 
    filter(dags == end_date[1]) |> 
    drop_na(per_pers) |> 
    mutate(
      colour = case_when(
        land == "Ísland" ~ litur_island,
        land == "Danmörk" ~ litur_danmork,
        land == "Finnland" ~ litur_finnland,
        land == "Noregur" ~ litur_noregur,
        land == "Svíþjóð" ~ litur_svithjod,
        TRUE ~ litur_annad
      ),
      linewidth = 1 * (land == "Ísland"),
      size = as_factor(linewidth),
      land_ordered = glue("<i style='color:{colour}'>{land}</i>"),
      land_ordered = fct_reorder(land_ordered, per_pers)
    ) |> 
    ggplot(aes(per_pers, land_ordered, col = colour, size = size)) +
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
      expand = expansion(c(0.15, 0.05)),
      breaks = breaks_extended(),
      limits = c(0, NA),
      labels = number_labels,
      guide = guide_axis_truncated(
        trunc_lower = 0
      )
    ) +
    scale_colour_identity() +
    scale_size_manual(values = c(1.5, 3)) +
    scale_linewidth(
      range = c(0.2, 0.4)
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
      title = "Fjöldi móttekinna umsókna",
      subtitle = glue("Fjöldi í {month(end_date[1], label = T, abbr = F)} {year(end_date)}")
    )
  
  plot_dat <- d |> 
    filter(name == "asylum_applicants_non_ukraine") |> 
    arrange(time) |> 
    rename(
      dags = time, 
      flottafjoldi = value,
      per_pers = per_pop
    ) |>   
    drop_na(per_pers) |> 
    select(dags, land, value = per_pers) |> 
    mutate(
      colour = case_when(
        land == "Ísland" ~ litur_island,
        land == "Danmörk" ~ litur_danmork,
        land == "Finnland" ~ litur_finnland,
        land == "Noregur" ~ litur_noregur,
        land == "Svíþjóð" ~ litur_svithjod,
        TRUE ~ litur_annad
      ),
      linewidth = 1 * (land == "Ísland"),
      size = as_factor(linewidth)
    )
  
  p12 <- plot_dat |> 
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
      breaks = breaks_extended(),
      labels = number_labels,
      limits = c(0, NA),
      expand = expansion(c(0, 0.01)),
      guide = guide_axis_truncated()
    ) +
    scale_colour_identity() +
    coord_cartesian(ylim = c(0, y_upper), clip = "on") +
    theme(
      plot.margin = margin(t = 5, r = 35, b = 5, l = 5)
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Þróun"
    )
  
  p1 <- p11 + p12 +
    plot_layout(nrow = 1, widths = c(0.8, 1))
  
  p21 <- d |> 
    filter(name == "total_decisions") |> 
    rename(
      dags = time, 
      flottafjoldi = value,
      per_pers = per_pop
    ) |> 
    filter(dags == end_date[2]) |> 
    drop_na(per_pers) |> 
    mutate(
      colour = case_when(
        land == "Ísland" ~ litur_island,
        land == "Danmörk" ~ litur_danmork,
        land == "Finnland" ~ litur_finnland,
        land == "Noregur" ~ litur_noregur,
        land == "Svíþjóð" ~ litur_svithjod,
        TRUE ~ litur_annad
      ),
      linewidth = 1 * (land == "Ísland"),
      size = as_factor(linewidth),
      land_ordered = glue("<i style='color:{colour}'>{land}</i>"),
      land_ordered = fct_reorder(land_ordered, per_pers)
    ) |> 
    ggplot(aes(per_pers, land_ordered, col = colour, size = size)) +
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
      expand = expansion(c(0.15, 0.05)),
      breaks = breaks_extended(),
      limits = c(0, NA),
      labels = number_labels,
      guide = guide_axis_truncated(
        trunc_lower = 0
      )
    ) +
    scale_colour_identity() +
    scale_size_manual(values = c(1.5, 3)) +
    scale_linewidth(
      range = c(0.2, 0.4)
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
      title = "Fjöldi afgreiddra umsókna",
      subtitle = glue("Fjöldi í {month(end_date[2], label = T, abbr = F)} {year(end_date)}")
    )
  
  plot_dat <- d |> 
    filter(name == "total_decisions") |> 
    arrange(time) |> 
    rename(
      dags = time, 
      flottafjoldi = value,
      per_pers = per_pop
    ) |>   
    drop_na(per_pers) |> 
    select(dags, land, value = per_pers) |> 
    mutate(
      colour = case_when(
        land == "Ísland" ~ litur_island,
        land == "Danmörk" ~ litur_danmork,
        land == "Finnland" ~ litur_finnland,
        land == "Noregur" ~ litur_noregur,
        land == "Svíþjóð" ~ litur_svithjod,
        TRUE ~ litur_annad
      ),
      linewidth = 1 * (land == "Ísland"),
      size = as_factor(linewidth)
    )
  
  p22 <- plot_dat |> 
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
      breaks = breaks_extended(),
      labels = number_labels,
      limits = c(0, NA),
      expand = expansion(c(0, 0.01)),
      guide = guide_axis_truncated()
    ) +
    scale_colour_identity() +
    coord_cartesian(ylim = c(0, y_upper), clip = "on") +
    theme(
      plot.margin = margin(t = 5, r = 35, b = 5, l = 5)
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Þróun"
    )
  
  p2 <- p21 + p22 +
    plot_layout(nrow = 1, widths = c(0.8, 1))
  
  p31 <- d |> 
    filter(name == "applicants_non_ukraine") |> 
    rename(
      dags = time, 
      flottafjoldi = value,
      per_pers = per_pop
    ) |> 
    filter(dags == end_date[3]) |> 
    drop_na(per_pers) |> 
    mutate(
      colour = case_when(
        land == "Ísland" ~ litur_island,
        land == "Danmörk" ~ litur_danmork,
        land == "Finnland" ~ litur_finnland,
        land == "Noregur" ~ litur_noregur,
        land == "Svíþjóð" ~ litur_svithjod,
        TRUE ~ litur_annad
      ),
      linewidth = 1 * (land == "Ísland"),
      size = as_factor(linewidth),
      land_ordered = glue("<i style='color:{colour}'>{land}</i>"),
      land_ordered = fct_reorder(land_ordered, per_pers)
    ) |> 
    ggplot(aes(per_pers, land_ordered, col = colour, size = size)) +
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
      expand = expansion(c(0.15, 0.05)),
      breaks = breaks_extended(),
      limits = c(0, NA),
      labels = number_labels,
      guide = guide_axis_truncated(
        trunc_lower = 0
      )
    ) +
    scale_colour_identity() +
    scale_size_manual(values = c(1.5, 3)) +
    scale_linewidth(
      range = c(0.2, 0.4)
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
      title = "Fjöldi einstaklinga í bið eftir niðurstöðu",
      subtitle = glue("Fjöldi í {month(end_date[3], label = T, abbr = F)} {year(end_date)}")
    )
  
  plot_dat <- d |> 
    filter(name == "applicants_non_ukraine") |> 
    arrange(time) |> 
    rename(
      dags = time, 
      flottafjoldi = value,
      per_pers = per_pop
    ) |>   
    drop_na(per_pers) |> 
    select(dags, land, value = per_pers) |> 
    mutate(
      colour = case_when(
        land == "Ísland" ~ litur_island,
        land == "Danmörk" ~ litur_danmork,
        land == "Finnland" ~ litur_finnland,
        land == "Noregur" ~ litur_noregur,
        land == "Svíþjóð" ~ litur_svithjod,
        TRUE ~ litur_annad
      ),
      linewidth = 1 * (land == "Ísland"),
      size = as_factor(linewidth)
    )
  
  p32 <- plot_dat |> 
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
      breaks = breaks_extended(),
      labels = number_labels,
      limits = c(0, NA),
      expand = expansion(c(0, 0.01)),
      guide = guide_axis_truncated()
    ) +
    scale_colour_identity() +
    coord_cartesian(ylim = c(0, y_upper), clip = "on") +
    theme(
      plot.margin = margin(t = 5, r = 35, b = 5, l = 5)
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Mánaðarleg þróun"
    )
  
  p3 <- p31 + p32 +
    plot_layout(nrow = 1, widths = c(0.8, 1))
  
  p <- p11 + p12 + p21 + p22 + p31 + p32 +
    plot_layout(ncol = 2) +
    plot_annotation(
      title = "Samantekt á umsóknum um hæli frá mars 2022",
      subtitle = str_c(
        "Þrátt fyrir að íslensk yfirvöld afgreiði mörg mál þá vantar meira upp á til að halda í við ",
        " fjölda umsókna."
      ),
      caption = caption
    ) &
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),
      plot.margin = margin(t = 0, r = 5, b = 0, l = 5)
    )
  
  girafe(
    ggobj = p,
    bg = "#f0efef",
    width_svg = width,
    height_svg = height,
    options = list(
      opts_tooltip(
        opacity = 0.8, 
        use_fill = TRUE,
        use_stroke = FALSE, 
        css = "padding:5pt;font-family: Open Sans;font-size:1rem;color:white"
      ),
      opts_hover(css = "", nearest_distance = 1000),
      opts_hover_inv(css = "opacity:0.05"), 
      opts_toolbar(saveaspng = TRUE),
      opts_zoom(max = 1)
    )
  )
}



