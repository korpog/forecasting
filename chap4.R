library(fpp3)
setwd("/home/korpog/Documents/prog3/forecasting")

# 4.1 Some simple statistics
tourism |>
  features(Trips, list(mean = mean)) |>
  arrange(mean)

tourism |> features(Trips, quantile)
