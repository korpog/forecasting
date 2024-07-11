library(fpp3)
setwd("/home/korpog/Documents/prog3/forecasting")

# 3.1 Transformations and adjustments
global_economy |>
  filter(Country == "Australia") |>
  autoplot(GDP / Population) +
  labs(title = "GDP per capita", y = "$US")

print_retail <- aus_retail |>
  filter(Industry == "Newspaper and book retailing") |>
  group_by(Industry) |>
  index_by(Year = year(Month)) |>
  summarise(Turnover = sum(Turnover))

aus_economy <- global_economy |>
  filter(Code == "AUS")

print_retail |>
  left_join(aus_economy, by = "Year") |>
  mutate(Adjusted_turnover = Turnover / CPI * 100) |>
  pivot_longer(c(Turnover, Adjusted_turnover),
    values_to = "Turnover"
  ) |>
  mutate(name = factor(name,
    levels = c("Turnover", "Adjusted_turnover")
  )) |>
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(
    title = "Turnover: Australian print media industry",
    y = "$AU"
  )

lambda <- aus_production |>
  features(Gas, features = guerrero) |>
  pull(lambda_guerrero)
aus_production |>
  autoplot(box_cox(Gas, lambda)) +
  labs(
    y = "",
    title = latex2exp::TeX(paste0(
      "Transformed gas production with $\\lambda$ = ",
      round(lambda, 2)
    ))
  )

# 3.2 Time series components
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)
autoplot(us_retail_employment, Employed) +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

dcmp <- us_retail_employment |>
  model(stl = STL(Employed))
components(dcmp)

components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = trend), colour = "#D55E00") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

components(dcmp) |> autoplot()

components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = "#0072B2") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

# 3.3 Moving averages
global_economy |>
  filter(Country == "Australia") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  mutate(
    `5-MA` = slider::slide_dbl(Exports, mean,
      .before = 2, .after = 2, .complete = TRUE
    )
  )

aus_exports |>
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(
    y = "% of GDP",
    title = "Total Australian exports"
  )

beer <- aus_production |>
  filter(year(Quarter) >= 1992) |>
  select(Quarter, Beer)
beer_ma <- beer |>
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
      .before = 1, .after = 2, .complete = TRUE
    ),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
      .before = 1, .after = 0, .complete = TRUE
    )
  )

us_retail_employment_ma <- us_retail_employment |>
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
      .before = 5, .after = 6, .complete = TRUE
    ),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
      .before = 1, .after = 0, .complete = TRUE
    )
  )

# 3.4 Classical decomposition
us_retail_employment_ma |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

# 3.5 Methods used by official statistics agencies
library(seasonal)
x11_dcmp <- us_retail_employment |>
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) |>
  components()
autoplot(x11_dcmp) +
  labs(
    title =
      "Decomposition of total US retail employment using X-11."
  )

x11_dcmp |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(
    y = season_adjust,
    colour = "Seasonally Adjusted"
  )) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  ) +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

x11_dcmp |>
  gg_subseries(seasonal)

seats_dcmp <- us_retail_employment |>
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) |>
  components()
autoplot(seats_dcmp) +
  labs(
    title =
      "Decomposition of total US retail employment using SEATS"
  )

# 3.6 STL decomposition
us_retail_employment |>
  model(
    STL(
      Employed ~ trend(window = 7) +
        season(window = "periodic"),
      robust = TRUE
    )
  ) |>
  components() |>
  autoplot()

# 3.7 exercises
View(global_economy)

global_economy |>
  filter(is.na(GDP) | is.na(Population) | Population == 0) |>
  mutate(GDP_Capita = GDP / Population) |>
  group_by(Country) |>
  summarise(MaxGDPC = max(GDP_Capita, na.rm = T)) |>
  arrange(desc(MaxGDPC)) |>
  head(5)

global_economy |>
  filter(Country == "United States") |>
  autoplot(GDP / Population) +
  labs(title = "GDP per capita", y = "$US")

aus_livestock |>
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") |>
  autoplot()

lambda <- aus_livestock |>
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") |>
  features(Count, features = guerrero) |>
  pull(lambda_guerrero)

aus_livestock |>
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") |>
  autoplot(box_cox(Count, lambda))

canadian_gas |>
  autoplot()

lambda_gas <- canadian_gas |>
  features(Volume, features = guerrero) |>
  pull(lambda_guerrero)

canadian_gas |>
  autoplot(box_cox(Volume, lambda_gas))

set.seed(12345678)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries |>
  autoplot()

lambda_m <- myseries |>
  features(Turnover, features = guerrero) |>
  pull(lambda_guerrero)

myseries |>
  autoplot(box_cox(Turnover, lambda_m))

tobacco <- aus_production |>
  select(Tobacco)

tobacco |> autoplot()

gas <- tail(aus_production, 5 * 4) |> select(Gas)
gas |> autoplot()

gas |>
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) |>
  components() |>
  autoplot()

gas_model <- gas |>
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) |>
  components()

gas_model |> 
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Gas, color = "data")) +
  geom_line(aes(y = season_adjust, color = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend"))

gas$Gas[20] = 500

gas_model <- gas |>
  model(
    classical_decomposition(Gas, type = "multiplicative")
  ) |>
  components()

gas_model |> 
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Gas, color = "data")) +
  geom_line(aes(y = season_adjust, color = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend"))


set.seed(12345678)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

x11_dcmp <- myseries |>
  model(x11 = X_13ARIMA_SEATS(Turnover ~ x11())) |>
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of total AUS retail using X-11.")

canadian_gas |> autoplot()
canadian_gas |> gg_subseries()
canadian_gas |> gg_season()


canadian_gas |>
  model(
    STL(Volume ~ trend(window = 7) +
          season(window = 365),
        robust = TRUE)) |>
  components() |>
  autoplot()

gas_model <- canadian_gas |> 
  model(
    STL(Volume ~ trend(window = 7) +
          season(window = 'periodic'),
        robust = TRUE)) |>
  components()

gas_model |> 
  ggplot(aes(x = Month)) +
  geom_line(aes(y = season_adjust, color = "Seasonally Adjusted"))
  