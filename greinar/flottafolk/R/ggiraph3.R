make_ggiraph3 <- function(
    width = 16,
    height = 0.4 * 16
) {
  
  n_countries <- length(unique(data_hist$land))
  plot_var <- "asylum_applicants"
  end_date <- max(data_hist$end_date, na.rm = T)
  label_function <- function(x) {
    if_else(
      year(x) == 2023,
      last_date_label,
      label_date_short(format = "%Y")(x)
    )
  }
  
  if (!is.finite(end_date)) {
    end_date <- max(data_hist$time)
    month(end_date) <- 12
    title_addon <- ""
    
    label_function <- function(x) {
      label_date_short(format = "%Y")(x)
    }
  } else {
    title_addon <- glue("[gögn fram að {month(end_date, label = T, abbr = F)}]")
  }
  last_date_label <- glue("2023\ntil {month(end_date, label = TRUE, abbr = FALSE)}")
  
  
  #### Plot 1 ####
  
  plot_dat <- data_hist |> 
    filter(
      name == plot_var
    ) |> 
    arrange(time, per_pop) |> 
    mutate(
      tooltip = glue(
        str_c(
          "Fjöldi (2022): {number(value[year(time)==2022], accuracy = 1, big.mark = '.', decimal.mark = ',')}\n",
          "Á 100.000 íbúa (2022): {number(per_pop[year(time)==2022], accuracy = 1, big.mark = '.', decimal.mark = ',')}\n",
          "Fjöldi (2023): {number(value[year(time)==2023], accuracy = 1, big.mark = '.', decimal.mark = ',')}\n",
          "Á 100.000 íbúa (2023): {number(per_pop[year(time)==2023], accuracy = 1, big.mark = '.', decimal.mark = ',')}"
        )
      ),
      tooltip = if_else(
        time == max(time),
        tooltip,
        NA_character_
      ),
      .by = land
    )
  
  
  
  p1 <- plot_dat |>  
    ggplot(aes(time, per_pop)) +
    geom_text_interactive(
      aes(
        x = clock::date_build(2008, 3),
        y = 1500,
        label = tooltip,
        data_id = land,
        colour = colour
      ),
      alpha = 0,
      size = 4,
      vjust = 1,
      hjust = 0
    ) +
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
      labels = label_function,
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
    coord_cartesian(ylim = c(0, 1700), clip = "off") +
    theme(
      plot.margin = margin(t = 5, r = 35, b = 5, l = 5),
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Fjöldi umsókna um hæli á höfðatölu",
      subtitle = "Sýnt sem fjöldi á 100.000 íbúa"
    )
  
  #### Plot 2 ####
  
  plot_dat <- data_hist |> 
    bind_rows(
      data_hist |> 
        filter(
          year(time) == 2023
        ) |> 
        mutate(
          time = time + years(1)
        )
    ) |> 
    filter(
      name == plot_var
    ) |> 
    arrange(time, per_pop) |> 
    mutate(
      placement = row_number(),
      .by = time
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
      breaks = clock::date_build(2008:2023, 7),
      limits = c(min(plot_dat$time), max(plot_dat$time) + years(2)),
      labels = label_function,
      expand = expansion(add = 50),
      guide = guide_axis_truncated(
        trunc_lower = clock::date_build(2008, 7),
        trunc_upper = clock::date_build(2023, 7)
      )
    ) +
    scale_y_continuous(
      breaks = seq_len(n_countries),
      labels = \(x) number(n_countries + 1 - x),
      limits = c(1, n_countries),
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
      title = "Evrópulöndum raðað eftir fjölda umsókna á höfðatölu",
      subtitle = "1: Flestar umsóknir | 30: Fæstar umsóknir"
    )
  
  
  #### Plot 3 ####
  
  plot_dat <- data_hist |> 
    filter(
      name == plot_var
    ) |> 
    arrange(time, per_pop) |> 
    mutate(
      per_pop = coalesce(per_pop, 0),
      value = coalesce(value, 0),
      per_pop = cumsum(per_pop),
      value = cumsum(value),
      .by = land
    ) |> 
    mutate(
      tooltip = glue(
        str_c(
          "Uppsafnaður fjöldi (2023): {number(value[year(time) == 2023], accuracy = 1, big.mark = '.', decimal.mark = ',')}\n",
          "Á 100.000 íbúa (2023): {number(per_pop[year(time) == 2023], accuracy = 1, big.mark = '.', decimal.mark = ',')}"
        )
      ),
      tooltip = if_else(
        time == max(time),
        tooltip,
        NA_character_
      ),
      .by = land
    )
  
  p3 <- plot_dat |>  
    ggplot(aes(time, per_pop)) +
    geom_text_interactive(
      aes(
        x = clock::date_build(2008, 3),
        y = 6000,
        label = tooltip,
        data_id = land,
        colour = colour
      ),
      alpha = 0,
      size = 4,
      vjust = 1,
      hjust = 0
    ) +
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
      labels = label_function,
      expand = expansion(add = 50),
      guide = guide_axis_truncated(
        trunc_lower = min(plot_dat$time),
        trunc_upper = max(plot_dat$time)
      )
    ) +
    scale_y_continuous(
      breaks = breaks_pretty(),
      labels = label_number(),
      expand = expansion(c(0.05, 0.05)),
      guide = guide_axis_truncated()
    )  +
    scale_colour_identity() +
    coord_cartesian(clip = "off", ylim = c(0, 6500)) +
    theme(
      plot.margin = margin(t = 5, r = 35, b = 5, l = 5),
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Uppsafnaður fjöldi hælisumsókna á höfðatölu",
      subtitle = "Sýnt sem fjöldi á 100.000 íbúa"
    )
  
  #### Plot 4 ####
  
  plot_dat <- data_hist |> 
    bind_rows(
      data_hist |> 
        filter(
          year(time) == 2023
        ) |> 
        mutate(
          time = time + years(1),
          per_pop = 0
        )
    ) |> 
    filter(
      name == plot_var
    ) |> 
    arrange(time) |> 
    mutate(
      per_pop = cumsum(per_pop),
      .by = land
    ) |> 
    arrange(per_pop) |> 
    mutate(
      placement = row_number(),
      .by = time
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
      breaks = clock::date_build(2008:2023, 7),
      limits = c(min(plot_dat$time), max(plot_dat$time) + years(2)),
      labels = label_function,
      expand = expansion(add = 50),
      guide = guide_axis_truncated(
        trunc_lower = clock::date_build(2008, 7),
        trunc_upper = clock::date_build(2023, 7)
      )
    ) +
    scale_y_continuous(
      breaks = seq_len(n_countries),
      labels = \(x) number(n_countries + 1 - x),
      limits = c(1, n_countries),
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
      subtitle = "1: Flestar umsóknir | 30: Fæstar umsóknir"
    )
  
  p <- p1 + p2 + p3 + p4 +
    plot_layout(nrow = 2) +
    plot_annotation(
      title = glue("Hælisumsóknir í Evrópulöndum (2008 til {year(end_date)} {title_addon})"),
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
      opts_hover(
        css = girafe_css(
          css = "",
          text = "stroke:none;fill-opacity:1;"
        ), 
        nearest_distance = 50
      ),
      opts_hover_inv(css = "opacity:0.05"), 
      opts_toolbar(saveaspng = TRUE),
      opts_zoom(max = 1)
    )
  ) 
  
}


