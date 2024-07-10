library(fpp3)
setwd("/home/korpog/Documents/prog3/forecasting")

# 2.1 tsibble objects
y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

prison <-
  readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison <- prison |>
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date) |>
  as_tsibble(
    key = c(State, Gender, Legal, Indigenous),
    index = Quarter
  )

PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6) -> a10

# 2.2 Time plots
melsyd_economy <- ansett |>
  filter(Airports == "MEL-SYD", Class == "Economy") |>
  mutate(Passengers = Passengers / 1000)
autoplot(melsyd_economy, Passengers) +
  labs(
    title = "Ansett airlines economy class",
    subtitle = "Melbourne-Sydney",
    y = "Passengers ('000)"
  )

autoplot(a10, Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

# 2.4 Seasonal plots
a10 |>
  gg_season(Cost, labels = "both") +
  labs(
    y = "$ (millions)",
    title = "Seasonal plot: Antidiabetic drug sales"
  )

vic_elec |> gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

vic_elec |> gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

# 2.5 Seasonal subseries plots
a10 |>
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))
holidays

autoplot(holidays, Trips) +
  labs(
    y = "Overnight trips ('000)",
    title = "Australian domestic holidays"
  )

gg_season(holidays, Trips) +
  labs(
    y = "Overnight trips ('000)",
    title = "Australian domestic holidays"
  )

holidays |>
  gg_subseries(Trips) +
  labs(
    y = "Overnight trips ('000)",
    title = "Australian domestic holidays"
  )

# 2.6 Scatterplots
vic_elec |>
  filter(year(Time) == 2014) |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(
    x = "Temperature (degrees Celsius)",
    y = "Electricity demand (GW)"
  )

visitors <- tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))
visitors |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(
    title = "Australian domestic tourism",
    y = "Overnight trips ('000)"
  )

visitors |>
  pivot_wider(values_from = Trips, names_from = State) |>
  GGally::ggpairs(columns = 2:9)

# 2.7 Lag plots
recent_production <- aus_production |>
  filter(year(Quarter) >= 2000)
recent_production |>
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

# 2.8 Autocorrelation
recent_production |>
  ACF(Beer) |>
  autoplot() + labs(title = "Australian beer production")

a10 |>
  ACF(Cost, lag_max = 48) |>
  autoplot() +
  labs(title = "Australian antidiabetic drug sales")

# 2.9 White noise
set.seed(30)
y <- tsibble(
  sample = 1:50,
  wn = rnorm(50),
  index = sample
)
y |> autoplot(wn) + labs(title = "White noise", y = "")

y |>
  ACF(wn) |>
  autoplot() + labs(title = "White noise")

# 2.10 (exercises)
autoplot(aus_production, Bricks) + labs(title = "Bricks")

autoplot(pelt, Lynx) + labs(title = "Lynx")

autoplot(gafa_stock, Close) + labs(title = "Close")

autoplot(vic_elec, Demand)
+labs(title = "Electricity Demand", x = "Date", y = "Electricity [MWh]")

gafa_stock |>
  group_by(Symbol) |>
  filter(Close == max(Close)) |>
  select(Close) -> maxes
maxes

tute1 <- readr::read_csv("tute1.csv")
View(tute1)

mytimeseries <- tute1 |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(index = Quarter)

mytimeseries |>
  pivot_longer(-Quarter) |>
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

library(USgas)
annual <- us_total |>
  as_tsibble(key = state, index = year)

new_england <-
  c(
    "Maine",
    "Vermont",
    "New Hampshire",
    "Massachusetts",
    "Connecticut",
    "Rhode Island"
  )

annual |>
  filter(state %in% new_england) |>
  ggplot(aes(x = year, y = y, colour = state)) +
  geom_line() +
  labs(
    title = "Annual consumption of gas by state",
    x = "Year",
    y = "Consumption [mcf]",
    color = "State"
  )

tourismX <- readxl::read_excel("tourism.xlsx")
tourismX <- tourismX |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(index = Quarter, key = c(Region, State, Purpose))

tourismX |>
  group_by(Region, Purpose) |>
  summarise(avg_trips = mean(Trips, na.rm = TRUE)) |>
  arrange(desc(avg_trips)) |>
  slice(1) -> avg_trips_b

avg_trips_b

state_totals <- tourismX |>
  index_by(Quarter) |>
  group_by(State) |>
  summarise(total_trips = sum(Trips, na.rm = FALSE)) |>
  ungroup() |>
  as_tsibble(key = State, index = Quarter)

aus_arrivals |> autoplot()
aus_arrivals |> gg_season()
aus_arrivals |> gg_subseries()

set.seed(14242)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries |> autoplot()
myseries |> gg_season()
myseries |> gg_subseries()
myseries |> gg_lag(lags = 1:12)
myseries |>
  ACF(Turnover, lag_max = 24) |>
  autoplot() + labs(title = "My series ACF plot")

View(us_employment)
emp <- us_employment |>
  filter(Title == "Total Private")

emp |> autoplot()
emp |> gg_season()
emp |> gg_subseries()
emp |> gg_lag(lags = 1:12)
emp |>
  ACF(Employed, lag_max = 24) |>
  autoplot() + labs(title = "Employment ACF plot")

aus_livestock |>
  filter(
    State == "Victoria",
    Animal == "Pigs",
    between(year(Month), 1990, 1995)
  ) -> pigs

pigs |> autoplot()

pigs |>
  ACF(Count, lag_max = 24) |>
  autoplot()

dgoog <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2018) |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index = trading_day, regular = TRUE) |>
  mutate(diff = difference(Close))

dgoog |> autoplot(diff)
dgoog |>
  ACF(diff, lag_max = 30) |>
  autoplot()
