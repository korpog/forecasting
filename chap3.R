library(fpp3)
setwd("/home/korpog/Documents/prog3/forecasting")

# 3.1
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

# 3.2
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

# 3.3
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
us_retail_employment_ma |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )
