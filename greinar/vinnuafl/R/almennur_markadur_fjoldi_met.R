library(tidyverse)
library(scales)
library(geomtextpath)
library(metill)
library(ggh4x)
library(ggforce)
theme_set(theme_metill(type = "standalone"))

d <- vroom::vroom("greinar/vinnuafl/data/vinnuafl.csv")

prev_record <- d |> 
  filter(bakgrunnur == "Alls", kyn == "Alls",
         rekstrarform != "Alls starfandi") |> 
  count(dags, tegund, wt = starfandi) |> 
  filter(
    tegund == "Annad",
    dags == clock::date_build(2018, 8)
  ) |> 
  pull(n)

min_2024 <- d |> 
  filter(bakgrunnur == "Alls", kyn == "Alls",
         rekstrarform != "Alls starfandi") |> 
  count(dags, tegund, wt = starfandi) |> 
  filter(
    tegund == "Annad",
    dags == clock::date_build(2024, 1)
  ) |> 
  pull(n)

d |> 
  filter(bakgrunnur == "Alls", kyn == "Alls",
         rekstrarform != "Alls starfandi") |> 
  count(dags, tegund, wt = starfandi) |> 
  filter(
    tegund == "Annad",
    dags == clock::date_build(2024, 3)
  ) |> 
  pull(n)

 p <- d |> 
  filter(bakgrunnur == "Alls", kyn == "Alls",
         rekstrarform != "Alls starfandi") |> 
  count(dags, tegund, wt = starfandi) |> 
  filter(tegund == "Annad") |>
  ggplot(aes(dags, n)) +
  geom_texthline(
    yintercept = prev_record, 
    lty = 2,
    label = "Starfandi í janúar 2024 voru bara 1% færri en í ágúst 2018",
    hjust = 0.05,
    size = 3,
    alpha = 0.5
  ) +
  geom_line() +
  geom_point(
    data = ~filter(.x, month(dags) == 1),
    col = "#377eb8",
    size = 2
  ) +
  geom_point(
    data = ~filter(.x, month(dags) == 8),
    col = "#e41a1c",
    size = 2
  ) +
  geom_mark_hull(
    aes(
      label = "Janúar",
      filter = (dags == clock::date_build(2021)),
      description = "En minnstur í janúar hvers árs"
    ),
    col = "#377eb8",
    label.fill = NA,
    con.arrow = arrow(type = "closed", length = unit(0.25, "cm"))
  ) +
  geom_mark_hull(
    aes(
      label = "Ágúst",
      filter = (dags == clock::date_build(2013, 7)),
      description = "Venjulega er fjöldi starfandi mestur í ágúst hvers árs"
    ),
    col = "#e41a1c",
    label.fill = NA,
    con.arrow = arrow(type = "closed", length = unit(0.25, "cm"))
  ) +
  scale_x_date(
    breaks = clock::date_build(2008:2024),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    labels = label_number(),
    breaks = breaks_extended(8),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Fjöldi starfandi á almennum vinnumarkaði er í methæðum",
    subtitle = "Að öllu óbreyttu verður minnsti fjöldi starfandi árið 2024 jafnhár og metfjöldinn fyrir COVID-19 faraldur",
    caption = str_c(
      "Mynd eftir @bggjonsson hjá metill.is byggð á gögnum Hagstofu um starfandi samkvæmt skrám\n",
      "Kóði: https://github.com/bgautijonsson/Metill.is/blob/master/greinar/vinnuafl/R/almennur_markadur_fjoldi_met.R"
    )
  )


ggsave(
  plot = p,
  filename = "greinar/vinnuafl/figures/almennur_fjoldi_methaedir.png",
  width = 8, height = 0.621 * 8, scale = 1.3
)







d |> 
  filter(
    bakgrunnur == "Alls",
    rekstrarform != "Alls starfandi",
    tegund == "Opinbert"
  ) |> 
  count(dags, tegund, wt = starfandi) |> 
  ggplot(aes(dags, n)) +
  geom_line() +
  geom_point(
    data = ~filter(.x, month(dags) == 7),
    col = "#e41a1c",
    size = 2
  ) +
  geom_mark_hull(
    aes(
      label = "Júlí",
      filter = (dags == clock::date_build(2013, 7)),
      description = "Venjulega er fjöldi starfandi mestur í júlí hvers árs"
    ),
    col = "#e41a1c",
    label.fill = NA,
    con.arrow = arrow(type = "closed", length = unit(0.25, "cm"))
  ) +
  scale_x_date(
    breaks = clock::date_build(2008:2024),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    labels = label_number(),
    breaks = breaks_extended(8),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Fjöldi starfandi á opinberum vinnumarkaði er í methæðum",
    caption = str_c(
      "Mynd eftir @bggjonsson hjá metill.is byggð á gögnum Hagstofu um starfandi samkvæmt skrám\n",
      "Kóði: https://github.com/bgautijonsson/Metill.is/blob/master/greinar/vinnuafl/R/almennur_markadur_fjoldi_met.R"
    )
  )


d |> 
  filter(
    bakgrunnur == "Alls",
    rekstrarform != "Alls starfandi",
    tegund == "Opinbert"
  ) |> 
  count(dags, tegund, wt = starfandi) |> 
  mutate(
    ar = year(dags),
    man = month(dags)
  ) |> 
  mutate(
    p = n / mean(n[man == 1]),
    .by = ar
  ) |> 
  ggplot(aes(man, p)) +
  geom_line(aes(group = ar))


d |> 
  filter(
    bakgrunnur == "Íslenskur bakgrunnur",
    rekstrarform != "Alls starfandi"
  ) |> 
  count(dags, tegund, wt = starfandi) |> 
  filter(tegund == "Annad") |>
  ggplot(aes(dags, n)) +
  geom_line() +
  geom_point(
    data = ~filter(.x, month(dags) == 1),
    col = "#377eb8",
    size = 2
  ) +
  geom_point(
    data = ~filter(.x, month(dags) == 8),
    col = "#e41a1c",
    size = 2
  ) +
  geom_mark_hull(
    aes(
      label = "Janúar",
      filter = (dags == clock::date_build(2021)),
      description = "En minnstur í janúar hvers árs"
    ),
    col = "#377eb8",
    label.fill = NA,
    con.arrow = arrow(type = "closed", length = unit(0.25, "cm"))
  ) +
  geom_mark_hull(
    aes(
      label = "Ágúst",
      filter = (dags == clock::date_build(2013, 7)),
      description = "Venjulega er fjöldi starfandi mestur í ágúst hvers árs"
    ),
    col = "#e41a1c",
    label.fill = NA,
    con.arrow = arrow(type = "closed", length = unit(0.25, "cm"))
  ) +
  scale_x_date(
    breaks = clock::date_build(2008:2024),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    labels = label_number(),
    breaks = breaks_extended(8),
    guide = guide_axis_truncated(),
    limits = c(85000, 140000)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Fjöldi starfandi á almennum vinnumarkaði er í methæðum",
    subtitle = "Að öllu óbreyttu verður minnsti fjöldi starfandi árið 2024 jafnhár og metfjöldinn fyrir COVID-19 faraldur",
    caption = str_c(
      "Mynd eftir @bggjonsson hjá metill.is byggð á gögnum Hagstofu um starfandi samkvæmt skrám\n",
      "Kóði: https://github.com/bgautijonsson/Metill.is/blob/master/greinar/vinnuafl/R/almennur_markadur_fjoldi_met.R"
    )
  )
