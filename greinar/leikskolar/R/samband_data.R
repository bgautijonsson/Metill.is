library(here)
library(ggh4x)

leikskolar <- here("greinar", "leikskolar", "data", "tenging-leikskolar-2002-2022.xlsx") |> 
  read_excel(
    sheet = "Heildagsígildi",
    skip = 5
  ) |> 
  fill(Rekstrarform, .direction = "down") |> 
  filter(
    !str_detect(Rekstrarform, "Samtals"),
    str_detect(Sveitarfelag, "samtals")
  ) |> 
  select(-1, -2, -4) |> 
  pivot_longer(c(-1), names_to = "ar", names_transform = as.numeric) |> 
  janitor::clean_names() |> 
  mutate(
    sveitarfelag = str_replace(sveitarfelag, "^[0-9]* ", "") |> 
      str_replace(" samtals", "") |> 
      str_squish()
  ) 


mannfjoldi |> 
  count(sveitarfelag, ar, wt = mannfjoldi) |> 
  inner_join(
    leikskolar |> 
      count(sveitarfelag, ar, wt = value, name = "leiksk")
  ) |> 
  filter(
    sveitarfelag %in% c(
      "Reykjavíkurborg",
      "Garðabær",
      "Kópavogsbær",
      "Hafnarfjarðarkaupstaður",
      "Seltjarnarnesbær"
      )
  ) |> 
  mutate(
    p = leiksk / n
  ) |> 
  ggplot(aes(ar, p, col = sveitarfelag)) +
  geom_line() +
  geom_text(
    data = ~filter(.x, ar == max(ar)),
    aes(label = sveitarfelag),
    hjust = 0,
    nudge_x = 0.1
  ) +
  scale_x_continuous(
    limits = c(2002, 2026)
  ) +
  theme(
    legend.position = "none"
  )





