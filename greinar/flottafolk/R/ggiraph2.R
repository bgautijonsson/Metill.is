make_ggiraph2 <- function(
    width = 16,
    height = 0.4 * 16
    ) {
  
  
  #### Plot 1 ####
  
  plot_dat <- data_hist |> 
    filter(
      land != "Bretland",
      name == "total_non_ukr"
    ) |> 
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
    ) |> 
    arrange(time, per_pop) |> 
    mutate(
      colour = if_else(
        land == "Lúxemborg",
        "black",
        colour
      )
    ) 
  
  p1 <- plot_dat |>  
    ggplot(aes(time, per_pop)) +
    geom_line_interactive(
      data = ~ filter(.x, colour == litur_annad),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 0.5
    ) +
    geom_line_interactive(
      data = ~ filter(.x, colour != litur_annad, land != "Ísland"),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 1
    ) +
    geom_line_interactive(
      data = ~ filter(.x, land == "Ísland"),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 1.5
    ) +
    scale_x_date(
      breaks = unique(plot_dat$time),
      limits = c(min(plot_dat$time), max(plot_dat$time) + years(2)),
      labels = label_date_short(format = "%Y"),
      expand = expansion(add = 50),
      guide = guide_axis_truncated(
        trunc_lower = min(plot_dat$time),
        trunc_upper = max(plot_dat$time)
      )
    ) +
    scale_y_continuous(
      breaks = breaks_pretty(),
      labels = label_number(),
      # limits = c(1, 22),
      expand = expansion(c(0.05, 0.05)),
      guide = guide_axis_truncated()
    )  +
    scale_colour_identity() +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(t = 5, r = 35, b = 5, l = 5),
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Fjöldi hælisveitinga á höfðatölu",
      subtitle = "Sýnt sem fjöldi á 100.000 íbúa"
    )
  
  #### Plot 2 ####
  
  plot_dat <- data_hist |> 
    bind_rows(
      data_hist |> 
        filter(
          year(time) == 2022
        ) |> 
        mutate(
          time = time + years(1)
        )
    ) |> 
    filter(
      land != "Bretland",
      name == "total_non_ukr"
    ) |> 
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
    ) |> 
    arrange(time, per_pop) |> 
    mutate(
      placement = row_number(),
      .by = time
    ) |> 
    mutate(
      colour = if_else(
        land == "Lúxemborg",
        "black",
        colour
      )
    ) 
  
  p2 <- plot_dat |>  
    ggplot(aes(time, placement)) +
    geom_step_interactive(
      data = ~ filter(.x, colour == litur_annad),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 0.3
    ) +
    geom_step_interactive(
      data = ~ filter(.x, colour != litur_annad, land != "Ísland"),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 1
    ) +
    geom_step_interactive(
      data = ~ filter(.x, land == "Ísland"),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 1.5
    ) +
    geom_text_interactive(
      data = ~ group_by(.x, land) |> 
        filter(land == "Ísland", time == max(time)),
      aes(label = land, colour = colour, data_id = land),
      hjust = 0,
      nudge_x = 30,
      size = 5
    ) +
    geom_text_interactive(
      data = ~ group_by(.x, land) |> 
        filter(colour != litur_annad, land != "Ísland", time == max(time)),
      aes(label = land, colour = colour, data_id = land),
      hjust = 0,
      nudge_x = 30
    ) +
    geom_text_interactive(
      data = ~ group_by(.x, land) |> 
        filter(colour == litur_annad, time == max(time)),
      aes(label = land, colour = colour, data_id = land),
      hjust = 0,
      nudge_x = 30,
      size = 3
    ) +
    scale_x_date(
      breaks = unique(plot_dat$time) + months(6),
      limits = c(min(plot_dat$time), max(plot_dat$time) + years(2)),
      labels = label_date_short(format = "%Y"),
      expand = expansion(add = 50),
      guide = guide_axis_truncated(
        trunc_lower = min(plot_dat$time) + months(6),
        trunc_upper = max(plot_dat$time) + months(6)
      )
    ) +
    scale_y_continuous(
      breaks = 1:22,
      labels = \(x) number(23 - x),
      limits = c(1, 22),
      expand = expansion(c(0.05, 0.05)),
      guide = guide_axis_truncated()
    )  +
    scale_colour_identity() +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(t = 5, r = 35, b = 5, l = 5),
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Evrópulöndum raðað eftir fjölda hælisveitinga á höfðatölu",
      subtitle = "1: Flestar veitingar | 23: Fæstar veitingar"
    )
  
  
  #### Plot 3 ####
  
  plot_dat <- data_hist |> 
    filter(
      land != "Bretland",
      name == "total_non_ukr"
    ) |> 
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
    ) |> 
    arrange(time, per_pop) |> 
    mutate(
      colour = if_else(
        land == "Lúxemborg",
        "black",
        colour
      )
    ) |> 
    mutate(
      per_pop = cumsum(per_pop),
      .by = land
    )
  
  p3 <- plot_dat |>  
    ggplot(aes(time, per_pop)) +
    geom_line_interactive(
      data = ~ filter(.x, colour == litur_annad),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 0.8
    ) +
    geom_line_interactive(
      data = ~ filter(.x, colour != litur_annad, land != "Ísland"),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 1.5
    ) +
    geom_line_interactive(
      data = ~ filter(.x, land == "Ísland"),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 2
    ) +
    scale_x_date(
      breaks = unique(plot_dat$time),
      limits = c(min(plot_dat$time), max(plot_dat$time) + years(2)),
      labels = label_date_short(format = "%Y"),
      expand = expansion(add = 50),
      guide = guide_axis_truncated(
        trunc_lower = min(plot_dat$time),
        trunc_upper = max(plot_dat$time)
      )
    ) +
    scale_y_continuous(
      breaks = breaks_pretty(),
      labels = label_number(),
      # limits = c(1, 22),
      expand = expansion(c(0.05, 0.05)),
      guide = guide_axis_truncated()
    )  +
    scale_colour_identity() +
    coord_cartesian(clip = "off", ylim = c(0, 4000)) +
    theme(
      plot.margin = margin(t = 5, r = 35, b = 5, l = 5),
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Uppsafnaður fjöldi hælisveitinga á höfðatölu",
      subtitle = "Sýnt sem fjöldi á 100.000 íbúa"
    )
  
  #### Plot 4 ####
  
  plot_dat <- data_hist |> 
    bind_rows(
      data_hist |> 
        filter(
          year(time) == 2022
        ) |> 
        mutate(
          time = time + years(1)
        )
    ) |> 
    filter(
      land != "Bretland",
      name == "total_non_ukr"
    ) |> 
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
    ) |> 
    mutate(
      per_pop = cumsum(per_pop),
      .by = land
    ) |> 
    arrange(time, per_pop) |> 
    mutate(
      placement = row_number(),
      .by = time
    ) |> 
    mutate(
      colour = if_else(
        land == "Lúxemborg",
        "black",
        colour
      )
    ) 
  
  p4 <- plot_dat |>  
    ggplot(aes(time, placement)) +
    geom_step_interactive(
      data = ~ filter(.x, colour == litur_annad),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 0.3
    ) +
    geom_step_interactive(
      data = ~ filter(.x, colour != litur_annad, land != "Ísland"),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 1
    ) +
    geom_step_interactive(
      data = ~ filter(.x, land == "Ísland"),
      aes(group = land, colour = colour, data_id = land),
      linewidth = 1.5
    ) +
    geom_text_interactive(
      data = ~ group_by(.x, land) |> 
        filter(land == "Ísland", time == max(time)),
      aes(label = land, colour = colour, data_id = land),
      hjust = 0,
      nudge_x = 30,
      size = 5
    ) +
    geom_text_interactive(
      data = ~ group_by(.x, land) |> 
        filter(colour != litur_annad, land != "Ísland", time == max(time)),
      aes(label = land, colour = colour, data_id = land),
      hjust = 0,
      nudge_x = 30
    ) +
    geom_text_interactive(
      data = ~ group_by(.x, land) |> 
        filter(colour == litur_annad, time == max(time)),
      aes(label = land, colour = colour, data_id = land),
      hjust = 0,
      nudge_x = 30,
      size = 3
    ) +
    scale_x_date(
      breaks = unique(plot_dat$time[year(plot_dat$time) != 2023]) + months(6),
      limits = c(min(plot_dat$time), max(plot_dat$time) + years(2)),
      labels = label_date_short(format = "%Y"),
      expand = expansion(add = 50),
      guide = guide_axis_truncated(
        trunc_lower = min(plot_dat$time) + months(6),
        trunc_upper = max(plot_dat$time) + months(6)
      )
    ) +
    scale_y_continuous(
      breaks = 1:22,
      labels = \(x) number(23 - x),
      limits = c(1, 22),
      expand = expansion(c(0.05, 0.05)),
      guide = guide_axis_truncated()
    )  +
    scale_colour_identity() +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(t = 5, r = 35, b = 5, l = 5),
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Evrópulöndum raðað eftir uppsöfnuðum fjölda frá 2008",
      subtitle = "1: Flestar veitingar | 23: Fæstar veitingar"
    )
 
  p <- p1 + p2 + p3 + p4 +
    plot_layout(nrow = 2) +
    plot_annotation(
      title = "Hælisveitingar í Evrópulöndum (2008 til 2022)",
      subtitle = str_c(
        "Láttu músina yfir land til að einblina á gögn þess",
        " | ",
        "Inniheldur ekki flóttafólk frá Úkraínu í kjölfar innrásar Rússlands"
      ),
      caption = caption
    ) &
    theme(
      plot.background = element_blank(),
      panel.background = element_blank()
    )
  
  girafe(
    ggobj = p,
    bg = "#f0efef",
    width_svg = width,
    height_svg = height,
    options = list(
      opts_tooltip(
        opacity = 0.8, 
        use_fill = FALSE,
        use_stroke = TRUE, 
        css = "padding:5pt;font-family: Open Sans;font-size:1rem;color:white"),
      opts_hover(css = "", nearest_distance = 50),
      opts_hover_inv(css = "opacity:0.05"), 
      opts_toolbar(saveaspng = TRUE),
      opts_zoom(max = 1)
    )
  ) 
  
}
