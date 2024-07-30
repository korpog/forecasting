library(fpp3)

# 7.1
us_change |>
  pivot_longer(c(Consumption, Income), names_to = "Series") |>
  autoplot(value) +
  labs(y = "% change")

us_change |>
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)", x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

us_change |>
  model(TSLM(Consumption ~ Income)) |>
  report()

us_change |>
  select(-Consumption, -Income) |>
  pivot_longer(-Quarter) |>
  ggplot(aes(Quarter, value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y = "% change")

us_change |>
  GGally::ggpairs(columns = 2:6)

# 7.2
fit_consMR <- us_change |>
  model(tslm = TSLM(Consumption ~ Income + Production +
    Unemployment + Savings))
report(fit_consMR)

augment(fit_consMR) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL, title = "Percent change in US consumption expenditure") +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = NULL))

augment(fit_consMR) |>
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(y = "Fitted (predicted values)", x = "Data (actual values)", title = "Percent change in US consumption expenditure") +
  geom_abline(intercept = 0, slope = 1)

# 7.3
fit_consMR |> gg_tsresiduals()

augment(fit_consMR) |>
  features(.innov, ljung_box, lag = 10)

us_change |>
  left_join(residuals(fit_consMR), by = "Quarter") |>
  pivot_longer(Income:Unemployment,
    names_to = "regressor",
    values_to = "x"
  ) |>
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

augment(fit_consMR) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  labs(x = "Fitted", y = "Residuals")

fit <- aus_airpassengers |>
  filter(Year <= 2011) |>
  left_join(guinea_rice, by = "Year") |>
  model(TSLM(Passengers ~ Production))
report(fit)

fit |> gg_tsresiduals()

# 7.4
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
recent_production |>
  autoplot(Beer) +
  labs(y = "Megalitres", title = "Australian quarterly beer production")

fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

augment(fit_beer) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  labs(y = "Megalitres", title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))

augment(fit_beer) |>
  ggplot(aes(
    x = Beer,
    y = .fitted,
    colour = factor(quarter(Quarter))
  )) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values", title = "Australian quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))

fourier_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + fourier(K = 2)))
report(fourier_beer)

# 7.5
glance(fit_consMR) |>
  select(adj_r_squared, CV, AIC, AICc, BIC)

# 7.6
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))
fc_beer <- forecast(fit_beer)
fc_beer |>
  autoplot(recent_production) +
  labs(title = "Forecasts of beer production using regression", y = "megalitres")

fit_consBest <- us_change |>
  model(lm = TSLM(Consumption ~ Income + Savings + Unemployment))
future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) |>
    mutate(
      Income = 1,
      Savings = 0.5,
      Unemployment = 0
    ),
  Decrease = new_data(us_change, 4) |>
    mutate(
      Income = -1,
      Savings = -0.5,
      Unemployment = 0
    ),
  names_to = "Scenario"
)

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change |>
  autoplot(Consumption) +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")

fit_cons <- us_change |>
  model(TSLM(Consumption ~ Income))
new_cons <- scenarios(
  "Average increase" = new_data(us_change, 4) |>
    mutate(Income = mean(us_change$Income)),
  "Extreme increase" = new_data(us_change, 4) |>
    mutate(Income = 12),
  names_to = "Scenario"
)
fcast <- forecast(fit_cons, new_cons)

us_change |>
  autoplot(Consumption) +
  autolayer(fcast) +
  labs(title = "US consumption", y = "% change")

# 7.7
boston_men <- boston_marathon |>
  filter(Year >= 1924) |>
  filter(Event == "Men's open division") |>
  mutate(Minutes = as.numeric(Time) / 60)

boston_men |> autoplot() +
  geom_smooth(method = "lm")


fit_trends <- boston_men |>
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980)))
  )
fc_trends <- fit_trends |> forecast(h = 10)

boston_men |>
  autoplot(Minutes) +
  geom_line(data = fitted(fit_trends), aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "Minutes", title = "Boston marathon winning times")

# exercises

# 1
jan14_vic_elec <- vic_elec |>
  filter(yearmonth(Time) == yearmonth("2014 Jan")) |>
  index_by(Date = as_date(Time)) |>
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  )

jan14_vic_elec |> autoplot()

fit_jan14 <- jan14_vic_elec |>
  model(TSLM(Demand ~ Temperature)) |>
  report()

fit_jan14 |> gg_tsresiduals()

new_cons <- scenarios(
  "15" = new_data(jan14_vic_elec, 1) |>
    mutate(Temperature = 15),
  "35" = new_data(jan14_vic_elec, 1) |>
    mutate(Temperature = 35),
  names_to = "Scenario"
)

fit_new <- jan14_vic_elec |>
  model(TSLM(Demand ~ Temperature)) |>
  forecast(new_data = new_cons)

jan14_vic_elec |>
  autoplot(Demand) +
  autolayer(fit_new, level = 95, alpha = 0.3)

intervals <- fit_new |>
  hilo(level = 95)

print(intervals)

vic_elec |>
  index_by(Date = as_date(Time)) |>
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  ) |>
  autoplot()

# 2
or <- olympic_running

or |> autoplot(Time)

fit_run <- or |>
  model(TSLM(Time ~ Year))

fit_run |>
  filter(Length == 800 & Sex == "men") |>
  report()

fit_run |>
  filter(Length == 800 & Sex == "men") |>
  gg_tsresiduals()

or_fc <- fit_run |>
  forecast(h = 1) |>
  hilo(level = 95)

# 4
suv <- souvenirs

suv |> autoplot()

suv <- suv |>
  mutate(
    surfing_festival = if_else(month(Month) == 3 & year(Month) >= 1988, 1, 0)
  )

suv_fit <- suv |>
  model(TSLM(log(Sales) ~ trend() + season() + surfing_festival))

report(suv_fit)

suv_fit |> gg_tsresiduals()

aug <- augment(suv_fit)

aug |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residuals")

aug |>
  ggplot(aes(x = month(Month, label = TRUE), y = .resid)) +
  geom_boxplot() +
  labs(x = "Month", y = "Residuals")

aug |>
  features(.innov, ljung_box, lag = 10)

future_data <- new_data(suv, n = 36) |>
  mutate(
    surfing_festival = if_else(month(Month) == 3, 1, 0)
  )

fc <- suv_fit |>
  forecast(new_data = future_data)

fc |>
  autoplot(suv) +
  labs(y = "Sales")

# 5
us_gas <- us_gasoline |>
  filter(year(Week) <= 2004)

us_gas |> autoplot()

gas_fit <- us_gas |>
  model(TSLM(Barrels ~ trend() + fourier(K = 7)))

glance(gas_fit) |>
  select(adj_r_squared, CV, AIC, AICc, BIC)

report(gas_fit)

aug_gas <- augment(gas_fit)

aug_gas |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = Barrels, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

gas_fit |> gg_tsresiduals()

gas_fc <- gas_fit |>
  forecast(h = 52)

gas_fc |>
  autoplot(us_gas)

# 6
afg <- global_economy |>
  filter(Country == "Afghanistan")

afg |> autoplot(Population)

afg_fit <- afg |>
  model(
    TSLM(Population ~ trend()),
    TSLM(Population ~ trend(knots = c(1980, 1989)))
  )

glance(afg_fit) |>
  select(adj_r_squared, CV, AIC, AICc, BIC)

afg_fc <- afg_fit |>
  forecast(h = 5)

afg_fc |>
  autoplot(afg)

mean <- afg_fc |>
  filter(Year == 2022)
