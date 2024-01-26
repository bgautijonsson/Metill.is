make_plot <- function(
    plot_var,
    scaling_var,
    start_date,
    end_date
) {
  d |> 
    filter(name == plot_var) |> 
    select(land, time, all_of(scaling_var)) |>
    pivot_wider(names_from = land, values_from = all_of(scaling_var)) |> 
    gt() |> 
    fmt_number(
      -1,
      decimals = 0
    ) |> 
      fmt_date(
        time,
        date_style = "yMMM",
        locale = "is"
      ) |> 
    opt_interactive()
}