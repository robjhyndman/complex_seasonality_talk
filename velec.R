#library(tidyverse)
#library(fpp3)

# Victorian Electricity Data
velec <- tsibbledata::vic_elec

# Convert to hourly
minute(velec$Time) <- 0
velec <- velec %>%
  group_by(Time) %>%
  summarise(
    Date = min(Date),
    Demand = sum(Demand)/1e3,
    Temperature = mean(Temperature),
    Holiday = any(Holiday),
    .groups = "drop"
  )

# Add some more variables and convert to tsibble
velec <- velec %>%
  mutate(
    Weekday = factor(
      weekdays(Date, abbreviate = TRUE),
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    ),
    WorkingDay = !Holiday & Weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"),
    DailySeasonality = as.numeric(Time - as_datetime(Date))/3600 + 11,
    WeeklySeasonality = (as.integer(Weekday) - 1) * 48 + DailySeasonality,
    WorkingDay = !Holiday & Weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"),
    WDSeasonality = (DailySeasonality + (!WorkingDay) * 100),
  ) %>%
  rename(DateTime = Time) %>%
  select(DateTime, Date, Demand, everything()) %>%
  as_tsibble(index=DateTime)

# msts version
velec_msts <- velec$Demand %>%
  forecast::msts(start = 2012, seasonal.periods=c(24, 24*7, 24*365))

