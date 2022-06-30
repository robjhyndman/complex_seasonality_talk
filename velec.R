# library(tidyverse)
# library(fpp3)

# Victorian Electricity Data
velec <- tsibbledata::vic_elec

# Convert to hourly
minute(velec$Time) <- 0
velec <- velec |>
  group_by(Time) |>
  summarise(
    Date = min(Date),
    Demand = sum(Demand) / 1e3,
    Temperature = mean(Temperature),
    Holiday = any(Holiday),
    .groups = "drop"
  )

# Add some more variables and convert to tsibble
velec <- velec |>
  mutate(
    Weekday = factor(
      weekdays(Date, abbreviate = TRUE),
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    ),
     WorkingDay = !Holiday & Weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri")
  ) |>
  rename(DateTime = Time) |>
  select(DateTime, Date, Demand, everything()) |>
  as_tsibble(index = DateTime)


# Remove Feb 29 to avoid leap year problems
velec_noly <- velec |>
  filter(Date != "2012-02-29")
velec_noly <- velec_noly |>
  mutate(
    DailySeasonality = rep(0:23, NROW(velec_noly) / 24),
    WeeklySeasonality = (as.integer(Weekday) - 1) * 24 + DailySeasonality,
    WDSeasonality = (DailySeasonality + (!WorkingDay) * 100),
    AnnualSeasonality = rep(0:8759, NROW(velec_noly) / 8760),
  )

# msts version
velec_msts <- velec_noly$Demand |>
  forecast::msts(start = 2012, seasonal.periods = c(24, 24 * 7, 24 * 365))
