make_plot <- function(
    plot_var,
    scaling_var,
    start_date,
    end_date,
    title,
    subtitle,
    caption,
    y_upper = NA,
    number_labels = label_number()
) {
  
  p1 <- d |> 
    filter(
      name == plot_var
    ) |> 
    rename(
      dags = time, 
      flottafjoldi = value,
      per_pers = scaling_var
    ) |> 
    filter(dags == min(dags)) |> 
    drop_na(flottafjoldi) |> 
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
      land = glue("<i style='color:{colour}'>{land}</i>"),
      land = fct_reorder(land, per_pers)
    ) |> 
    ggplot(aes(per_pers, land, col = colour, size = size)) +
    geom_point() +
    geom_segment(aes(yend = land, xend = 0, linewidth = linewidth), lty = 2, alpha = 0.5) +
    scale_x_continuous(
      expand = expansion(c(0, 0.05)),
      breaks = breaks_extended(),
      limits = c(0, NA),
      labels = number_labels
    ) +
    scale_colour_identity() +
    scale_size_manual(values = c(1, 3)) +
    scale_linewidth(
      range = c(0.2, 0.4)
    ) +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(t = 5, r = 25, b = 5, l = 5),
      axis.text.y = element_markdown(size = 10, family = "Lato"),
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = glue("Fjöldi í {month(start_date, label = T, abbr = F)} {year(start_date)}"),
      caption = caption
    )
  
  
  p2 <- d |> 
    filter(name == plot_var) |> 
    rename(
      dags = time, 
      flottafjoldi = value,
      per_pers = scaling_var
    ) |> 
    filter(dags == end_date) |> 
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
      land = glue("<i style='color:{colour}'>{land}</i>"),
      land = fct_reorder(land, per_pers)
    ) |> 
    ggplot(aes(per_pers, land, col = colour, size = size)) +
    geom_point() +
    geom_segment(
      aes(yend = land, xend = 0, linewidth = linewidth),
      lty = 2, 
      alpha = 0.5
    ) +
    scale_x_continuous(
      expand = expansion(c(0, 0.05)),
      limits = c(0, NA),
      labels = number_labels
    ) +
    scale_colour_identity() +
    scale_size_manual(values = c(1, 3)) +
    scale_linewidth(
      range = c(0.2, 0.4)
    ) +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(t = 5, r = 25, b = 5, l = 5),
      axis.text.y = element_markdown(size = 10),
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = glue("Fjöldi í {month(end_date, label = T, abbr = F)} {year(end_date)}"),
      caption = caption
    )
  
  plot_dat <- d |> 
    filter(name == plot_var) |> 
    arrange(time) |> 
    rename(
      dags = time, 
      flottafjoldi = value,
      per_pers = scaling_var
    ) |>  
    # filter(dags <= end_date) |> 
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
  
  p3 <- plot_dat |> 
    ggplot(aes(dags, value)) +
    geom_line(
      data = plot_dat |> 
        filter(colour == litur_annad),
      aes(group = land, colour = litur_annad),
      alpha = 0.3,
      col = litur_annad
    ) +
    geom_line(
      data = plot_dat |> 
        filter(colour != litur_annad),
      aes(group = land, colour = colour),
      linewidth = 1
    ) +
    ggrepel::geom_text_repel(
      data = plot_dat |> 
        group_by(land) |> 
        filter(colour != litur_annad, dags == max(dags)) |> 
        ungroup(),
      aes(label = land, colour = colour),
      hjust = 0.1,
      nudge_x = 15,
      direction = "y", 
      min.segment.length = Inf,
      force = 0.1,
      force_pull = 2
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
      subtitle = "Fjöldi á mánuði"
    )
  
  p <- (
    (p1 + labs(title = NULL, caption = NULL)) + 
      (p2 + labs(title = NULL, caption = NULL))
  ) / 
    p3 +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = caption
    )
  
  p
}



