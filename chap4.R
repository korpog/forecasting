library(fpp3)
library(GGally)
setwd("/home/korpog/Documents/prog3/forecasting")

tourism
# 4.1 Some simple statistics
tourism |>
  features(Trips, list(mean = mean)) |>
  arrange(mean)

tourism |> features(Trips, quantile)

# 4.2 ACF features
tourism |> features(Trips, feat_acf)

# 4.3 STL Features
tourism |>
  features(Trips, feat_stl)

tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(
    x = trend_strength, y = seasonal_strength_year,
    col = Purpose
  )) +
  geom_point() +
  facet_wrap(vars(State))

tourism |>
  features(Trips, feat_stl) |>
  filter(
    seasonal_strength_year == max(seasonal_strength_year)
  ) |>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose))

# 4.5 Exploring Australian tourism data
tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts"))
tourism_features


library(glue)
tourism_features |>
  select_at(vars(contains("season"), Purpose)) |>
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4 * (seasonal_peak_year == 0),
    seasonal_trough_year = seasonal_trough_year +
      4 * (seasonal_trough_year == 0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) |>
  GGally::ggpairs(mapping = aes(colour = Purpose))

library(broom)
pcs <- tourism_features |>
  select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE) |>
  augment(tourism_features)
pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)

outliers <- pcs |>
  filter(.fittedPC1 > 10) |>
  select(Region, State, Purpose, .fittedPC1, .fittedPC2)
outliers

outliers |>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
  mutate(Series = glue("{State}", "{Region}", "{Purpose}", .sep = "\n\n")) |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in PC space")

# 4.6 exercises
View(PBS)

mean_sd <- function(data) {
  mean <- mean(data, na.rm = T)
  sd <- sd(data, na.rm = T)
  return(list(m = mean, s = sd))
}

pbs_stats <- PBS |>
  group_by(ATC1) |>
  index_by(Month) |>
  summarise(
    stats = list(mean_sd(Cost)),
    .groups = "drop"
  ) |>
  mutate(
    mean = sapply(stats, function(x) x$m),
    sd = sapply(stats, function(x) x$s)
  ) |>
  select(-stats)

pbs_s <- PBS |>
  features(Cost, list(mean = mean, stdev = sd))

max_mean <- pbs_s |> slice_max(mean, n = 1)
min_sd <- pbs_s |> slice_min(stdev, n = 1)

PBS |>
  filter(
    Concession == "Concessional", Type == "Co-payments", ATC1 == 1,
    ATC2 == 10
  ) |>
  autoplot(Cost)

tourism_features <- tourism |>
  filter(Purpose == "Holiday") |>
  features(Trips, feat_stl) |>
  mutate(
    State = factor(State),
    seasonal_peak_year = factor(seasonal_peak_year),
    seasonal_trough_year = factor(seasonal_trough_year)
  )

ggpairs(tourism_features,
  columns = c(
    "trend_strength", "seasonal_strength_year",
    "seasonal_peak_year", "seasonal_trough_year"
  ),
  mapping = aes(color = State, alpha = 0.6),
  upper = list(continuous = wrap("cor", size = 2.5))
) +
  theme_minimal()

peak_quarters <- tourism_features |>
  group_by(State) |>
  summarise(Peak_Quarter = seasonal_peak_year[1])

print(peak_quarters)
PBS
pbs_features <- PBS |>
  features(Cost, feature_set(pkgs = "feasts")) |>
  select(-...26)

pbs_features

library(broom)
pcs <- pbs_features |>
  select(-Concession, -Type, -ATC1, -ATC2) |>
  prcomp(scale = TRUE) |>
  augment(pbs_features)

pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)
