library(fpp3)

# 5.1 A tidy forecasting workflow
gdppc <- global_economy |>
  mutate(GDP_per_capita = GDP / Population)

gdppc |>
  filter(Country == "Sweden") |>
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")

fit <- gdppc |>
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit |> forecast(h = "3 years")

fit |>
  forecast(h = "3 years") |>
  filter(Country == "Sweden") |>
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")

# 5.2 Some simple forecasting methods
bricks <- aus_production |>
  filter_index("1970 Q1" ~ "2004 Q4") |>
  select(Bricks)

bricks |> model(MEAN(Bricks))
bricks |> model(NAIVE(Bricks))
bricks |> model(SNAIVE(Bricks ~ lag("year")))
bricks |> model(RW(Bricks ~ drift()))

# Set training data from 1992 to 2006
train <- aus_production |>
  filter_index("1992 Q1" ~ "2006 Q4")
# Fit the models
beer_fit <- train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )
# Generate forecasts for 14 quarters
beer_fc <- beer_fit |> forecast(h = 14)
# Plot forecasts against actual values
beer_fc |>
  autoplot(train, level = NULL) +
  autolayer(filter_index(aus_production, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# Re-index based on trading days
google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)
# Fit the models
google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )
# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock |>
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit |>
  forecast(new_data = google_jan_2016)
# Plot the forecasts
google_fc |>
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(
    y = "$US",
    title = "Google daily closing stock prices",
    subtitle = "(Jan 2015 - Jan 2016)"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# 5.3 Fitted values and residuals
augment(beer_fit)

# 5.4 Residual diagnostics
autoplot(google_2015, Close) +
  labs(
    y = "$US",
    title = "Google daily closing stock prices in 2015"
  )

aug <- google_2015 |>
  model(NAIVE(Close)) |>
  augment()
autoplot(aug, .innov) +
  labs(
    y = "$US",
    title = "Residuals from the naïve method"
  )

aug |>
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")

aug |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residuals from the naïve method")


google_2015 |>
  model(NAIVE(Close)) |>
  gg_tsresiduals()

aug |> features(.innov, box_pierce, lag = 10)
aug |> features(.innov, ljung_box, lag = 10)

fit <- google_2015 |> model(RW(Close ~ drift()))
tidy(fit)
augment(fit) |> features(.innov, ljung_box, lag = 10)

# 5.5 Distributional forecasts and prediction intervals
google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10) |>
  hilo()

fit <- google_2015 |>
  model(NAIVE(Close))
sim <- fit |> generate(
  h = 30,
  times = 5,
  bootstrap = TRUE
)
sim

google_2015 |>
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
    data = sim
  ) +
  labs(title = "Google daily closing stock price", y = "$US") +
  guides(colour = "none")

fc <- fit |> forecast(h = 30, bootstrap = TRUE)
fc

autoplot(fc, google_2015) +
  labs

google_2015 |>
  model(NAIVE(Close)) |>
  forecast(
    h = 10,
    bootstrap = TRUE,
    times = 1000
  ) |>
  hilo()

# 5.6 Forecasting using transformations
fc <- prices |>
  filter(!is.na(eggs)) |>
  model(RW(log(eggs) ~ drift())) |>
  forecast(h = 50) |>
  mutate(.median = median(eggs))
fc |>
  autoplot(prices |> filter(!is.na(eggs)), level = 80, ) +
  geom_line(
    aes(y = .median),
    data = fc,
    linetype = 3,
    col = "blue"
  ) +
  labs(
    title = "Annual egg prices",
    y = "$US (in cents adjusted for inflation) "
  )

# 5.7 Forecasting with decomposition
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade")
dcmp <- us_retail_employment |>
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) |>
  components() |>
  select(-.model)
dcmp |>
  model(NAIVE(season_adjust)) |>
  forecast() |>
  autoplot(dcmp) +
  labs(
    y = "Number of people",
    title = "US retail employment"
  )

fit_dcmp <- us_retail_employment |>
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))
fit_dcmp |>
  forecast() |>
  autoplot(us_retail_employment) +
  labs(
    y = "Number of people",
    title = "US retail employment"
  )

fit_dcmp |> gg_tsresiduals()

# 5.8 Evaluating point forecast accuracy
aus_production |> filter(year(Quarter) >= 1995)
aus_production |> filter_index("1995 Q1" ~ .)

aus_production |>
  slice(n() - 19:0)

aus_retail |>
  group_by(State, Industry) |>
  slice(1:12)

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
beer_train <- recent_production |>
  filter(year(Quarter) <= 2007)

beer_fit <- beer_train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit |>
  forecast(h = 10)

beer_fc |>
  autoplot(aus_production |> filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(beer_fc, recent_production)

google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )

google_fc <- google_fit |>
  forecast(google_jan_2016)
google_fc |>
  autoplot(bind_rows(google_2015, google_jan_2016),
    level = NULL
  ) +
  labs(
    y = "$US",
    title = "Google closing stock prices from Jan 2015"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(google_fc, google_stock)

# 5.9 Evaluating distributional forecast accuracy
google_fc |>
  filter(.model == "Naïve") |>
  autoplot(bind_rows(google_2015, google_jan_2016), level = 80) +
  labs(
    y = "$US",
    title = "Google closing stock prices"
  )

google_fc |>
  filter(.model == "Naïve", Date == "2016-01-04") |>
  accuracy(google_stock, list(qs = quantile_score), probs = 0.10)

google_fc |>
  filter(.model == "Naïve", Date == "2016-01-04") |>
  accuracy(google_stock,
    list(winkler = winkler_score),
    level = 80
  )

google_fc |>
  accuracy(google_stock, list(crps = CRPS))

google_fc |>
  accuracy(google_stock, list(skill = skill_score(CRPS)))

# 5.10 Time series cross-validation
google_2015_tr <- google_2015 |>
  stretch_tsibble(.init = 3, .step = 1) |>
  relocate(Date, Symbol, .id)
google_2015_tr

# TSCV accuracy
google_2015_tr |>
  model(RW(Close ~ drift())) |>
  forecast(h = 1) |>
  accuracy(google_2015)
# Training set accuracy
google_2015 |>
  model(RW(Close ~ drift())) |>
  accuracy()

google_2015_tr <- google_2015 |>
  stretch_tsibble(.init = 3, .step = 1)
fc <- google_2015_tr |>
  model(RW(Close ~ drift())) |>
  forecast(h = 8) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Close", distribution = Close)
fc |>
  accuracy(google_2015, by = c("h", ".model")) |>
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()

# 5.11 exercises
ge <- global_economy
aus <- global_economy |>
  filter(Country == "Australia") |>
  select(Country, Population)

aus |> autoplot()

train <- aus |>
  filter_index(1960 ~ 2007)

test <- aus |>
  filter_index(2008 ~ .)

aus_fit <- train |>
  model(`Drift` = RW(Population ~ drift()))

aus_fc <- aus_fit |>
  forecast(h = 10)

aus_fc |>
  autoplot(aus, level = 95) +
  autolayer(test, Population, colour = "red") +
  guides(colour = guide_legend(title = "Forecast"))

g <- gafa_stock

fb <- g |>
  filter(Symbol == "FB", year(Date) >= 2014) |>
  select(Date, Symbol, Close) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)

fb_2014 <- fb |>
  filter(year(Date) == 2014)

fb_jan2015 <- fb |>
  filter(yearmonth(Date) == yearmonth("2015 Jan"))

fb_fit <- fb_2014 |>
  model(
    `Drift` = RW(Close ~ drift()),
    `NAIVE` = NAIVE(Close)
  )

fb_fc <- fb_fit |>
  forecast(new_data = fb_jan2015)

fb_fc |>
  autoplot(fb_2014, level = 80) +
  autolayer(fb_jan2015, Close, color = "red")

first_last <- fb_2014 |>
  slice(c(1, n()))

fb_fc |>
  autoplot(fb_2014, level = 80) +
  geom_line(data = first_last, aes(x = day, y = Close), color = "green") +
  geom_point(
    data = first_last,
    aes(x = day, y = Close),
    color = "red",
    size = 3
  )

# Extract data of interest
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
# Define and estimate a model
fit <- recent_production |> model(SNAIVE(Beer))
# Look at the residuals
fit |> gg_tsresiduals()
# Look a some forecasts
fit |>
  forecast() |>
  autoplot(recent_production)

set.seed(12345678)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries_train <- myseries |>
  filter(year(Month) < 2011)

autoplot(myseries, Turnover) +
  autolayer(myseries_train, Turnover, colour = "red")

fit <- myseries_train |>
  model(SNAIVE(Turnover))

fit |> gg_tsresiduals()

fc <- fit |>
  forecast(new_data = anti_join(myseries, myseries_train))
fc |> autoplot(myseries)

fit |> accuracy()
fc |> accuracy(myseries)

al <- aus_livestock
al
unique(al$State)

pigs <- al |>
  filter(Animal == "Pigs", State == "New South Wales")

pigs |> autoplot()

pigs_train <- pigs |>
  slice(1:486)

pigs_test <- pigs |>
  slice(487:nrow(pigs))

fit <- pigs_train |>
  model(
    MEAN(Count),
    NAIVE(Count),
    RW(Count ~ drift())
  )

fc <- fit |>
  forecast(new_data = pigs_test)

fc |> autoplot(pigs, level = 80)

accuracy(fc, pigs)

fit$.model
fit

pigs_train |>
  model(RW(Count ~ drift())) |>
  gg_tsresiduals()

# 11

bricks <- aus_production |>
  select(Quarter, Bricks) |>
  filter(!is.na(Bricks))

bricks_stl <- bricks |>
  model(
    STL(
      Bricks ~ trend(window = 7) +
        season(window = "periodic"),
      robust = TRUE
    )
  ) |>
  components()

bricks_stl |> autoplot()

ggplot(bricks_stl, aes(x = Quarter)) +
  geom_line(aes(y = season_adjust))

bricks_fit <- bricks_stl |>
  select(Quarter, season_adjust) |>
  model(NAIVE(season_adjust))

bricks_fc <- bricks_fit |>
  forecast(h = 8)

fc_reseason <- bricks |>
  model(stl_dcmp = decomposition_model(
    STL(
      Bricks ~ trend(window = 7) +
        season(window = "periodic"),
      robust = TRUE
    ),
    NAIVE(season_adjust)
  )) |>
  forecast(h = 8)

autoplot(bricks) + autolayer(fc_reseason)

test_set <- bricks |>
  slice_tail(n = 6)

fc_dcmp <- bricks |>
  model(stl_dcmp = decomposition_model(
    STL(
      Bricks ~ trend(window = 7) +
        season(window = "periodic"),
      robust = TRUE
    ),
    NAIVE(season_adjust)
  )) |>
  forecast(new_data = test_set)

fc_snaive <- bricks |>
  model(SNAIVE(Bricks)) |>
  forecast(new_data = test_set)

accuracy(fc_snaive, bricks)
accuracy(fc_dcmp, bricks)


# 12
gc_tourism <- tourism |>
  filter(Region == "Gold Coast") |>
  group_by(Purpose) |>
  summarise(total_trips = sum(Trips))

gc_train_1 <- gc_tourism |> slice(1:(n() - 4))
gc_train_2 <- gc_tourism |> slice(1:(n() - 8))
gc_train_3 <- gc_tourism |> slice(1:(n() - 12))

gc_fc_1 <- gc_train_1 |>
  model(SNAIVE(total_trips)) |>
  forecast(h = 4) |>
  autoplot()

gc_fc_2 <- gc_train_2 |>
  model(SNAIVE(total_trips)) |>
  forecast(h = 8)

gc_fc_3 <- gc_train_3 |>
  model(SNAIVE(total_trips)) |>
  forecast(h = 12)

accuracy(gc_fc_1, gc_tourism)
accuracy(gc_fc_2, gc_tourism)
accuracy(gc_fc_3, gc_tourism)
