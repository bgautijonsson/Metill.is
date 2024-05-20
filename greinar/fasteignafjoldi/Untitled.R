library(ggh4x)
read_parquet("greinar/fasteignafjoldi/data/data_iceland.parquet") |> 
  mutate(
    fasteignir_per_pop = fasteignir / mannfjoldi * 1000,
    fpp_rolling = slider::slide_dbl(fasteignir_per_pop, sum, .before = 9)
  ) |> 
  filter(
    ar >= 2007,
    ar < 2024
  ) |> 
  ggplot(aes(ar, fpp_rolling)) +
  geom_line(
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = seq(2007, 2023, by = 2),
    limits = c(2007, 2023),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    breaks = breaks_extended(10),
    limits = c(0, NA),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Á Íslandi eru byggðar fleiri nýjar fasteignir en í flestum öðrum löndum",
    subtitle = "Eftir stöðuga afturför eftir fjármálahrunið hefur nýbyggingum farið fjöldandi frá 2018"
  )

read_parquet("greinar/fasteignafjoldi/data/data.parquet") |> 
  select(ar, sveitarfelag, fasteignir = fjoldi, mannfjoldi) |> 
  mutate(
    fasteignir_per_pop = fasteignir / mannfjoldi * 1000,
    fpp_rolling = slider::slide_dbl(fasteignir_per_pop, sum, .before = 9),
    .by = sveitarfelag
  ) |> 
  filter(ar == 2023) |> 
  mutate(
    sveitarfelag = fct_reorder(sveitarfelag, fpp_rolling)
  ) |> 
  ggplot(aes(fpp_rolling, sveitarfelag)) +
  geom_labelvline(
    xintercept = 70.8,
    lty = 2,
    alpha = 0.4,
    linewidth = 0.4,
    size = 5,
    label = "Ísland í heild",
    inherit.aes = FALSE
  ) +
  geom_segment(
    aes(yend = sveitarfelag, xend = 0),
    linewidth = 0.3,
    alpha = 0.3
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    limits = c(0, NA),
    expand = expansion(c(0, 0.1)),
    breaks = breaks_extended(14),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = "Fjöldi nýbyggðra fasteigna á 1.000 íbúa (2014 til 2023)",
    y = NULL,
    title = "Fjöldi nýbyggðra fasteigna á 1.000 íbúa í sveitarfélögum landsins",
    subtitle = "Sýnt sem heildarfjöldi frá 2014 til 2023"
  )
