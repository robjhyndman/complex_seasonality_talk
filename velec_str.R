# Fit stR model
# Assumes velec.R has already been run

library(stR)
library(distributional)

# Create predictor dist object
predictor_dist <- function(x, level) {
  dist_percentile(
    with(x, mapply(c, data, split(lower, row(lower)), split(upper, row(upper)), SIMPLIFY = FALSE)),
    list(c(50, 50 + rep(c(-1, 1), each = length(level)) * rep(level, 2) / 2))
  )
}

# Fit str with no covariates
if(file.exists("velec_str.rds")) {
  velec_str <- readRDS("velec_str.rds")
} else {
  velec_str <- AutoSTR(velec_msts, gapCV = 24 * 7)
  saveRDS(velec_str, "velec_str.rds")
}


# Set up predictors for STR with covariates
Predictors <- list()
Predictors$Trend <- list(
  name = "Trend",
  data = rep(1, NROW(velec)),
  times = velec$DateTime,
  seasons = rep(1, NROW(velec)),
  timeKnots = seq(from = first(velec$DateTime), to = last(velec$DateTime), length.out = 58),
  seasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0))),
  lambdas = c(1500, 0, 0)
)
Predictors$WSeason <- list(
  name = "Weekly seas",
  data = rep(1, NROW(velec)),
  times = velec$DateTime,
  seasons = velec$WeeklySeasonality,
  timeKnots = seq(from = first(velec$DateTime), to = last(velec$DateTime), length.out = 6),
  seasonalStructure = list(segments = list(c(0, 168)), sKnots = c(as.list(seq(4, 166, 4)), list(c(168, 0)))),
  lambdas = c(0.8, 0.6, 100)
)
Predictors$WDSeason <- list(
  name = "Daily seas",
  data = rep(1, NROW(velec)),
  times = velec$DateTime,
  seasons = velec$WDSeasonality,
  timeKnots = seq(from = first(velec$DateTime), to = last(velec$DateTime), length.out = 12),
  seasonalStructure = list(
    segments = list(c(0, 24), c(50, 74)),
    sKnots = c(as.list(c(1:47, 101:147)), list(c(0, 48, 100, 148)))
  ),
  lambdas = c(0.003, 0, 240)
)
Predictors$TrendTempM <- list(
  name = "Trend temp",
  data = velec$Temp,
  times = velec$DateTime,
  seasons = rep(1, NROW(velec)),
  timeKnots = Predictors$Trend$timeKnots,
  seasonalStructure = Predictors$Trend$seasonalStructure,
  lambdas = c(1e7, 0, 0)
)
Predictors$TrendTempM2 <- list(
  name = "Trend temp^2",
  data = velec$Temp^2,
  times = velec$DateTime,
  seasons = rep(1, NROW(velec)),
  timeKnots = Predictors$Trend$timeKnots,
  seasonalStructure = Predictors$Trend$seasonalStructure,
  lambdas = c(1e7, 0, 0)
)

# STR decomposition of electricity data
velec_str_x <- STR(
  data = velec$Demand,
  predictors = Predictors,
  gapCV = 24 * 7
)

# Find outliers
velec <- velec %>%
  mutate(res = elec_str$output$random$data)
outliers <- velec %>%
  filter(abs(res) >= sort(abs(velec$res))[NROW(velec) - 10 + 1])

## Plot STR decomposition of electricity data

elec_dbl <- velec %>%
  as_tsibble(index = DateTime) %>%
  mutate(
    Remainder = elec_str$output$random$data,
    Trend = predictor_dist(elec_str$output$predictors[[1]], 95),
    Weekly = predictor_dist(elec_str$output$predictors[[2]], 95),
    Daily = predictor_dist(elec_str$output$predictors[[3]], 95),
    Temp = predictor_dist(elec_str$output$predictors[[4]], 95),
    Tempsq = predictor_dist(elec_str$output$predictors[[5]], 95),
  ) %>%
  select(DateTime, Demand, Trend, Weekly, Daily, Temp, Tempsq, Remainder) %>%
  as_dable(
    response = "Demand",
    aliases = rlang::exprs(Demand = Trend + Weekly + Daily + Temp + Tempsq + Remainder),
    method = "STR"
  )
p <- elec_dbl %>%
  autoplot(size = 0.5, fill = "blue", level = 95) +
  xlab("Month") +
  geom_vline(data = outliers, aes(xintercept = DateTime), col = "gray") +
  ggplot2::theme_bw() + theme(legend.position = "none")
# Put vline behind data
p$layers <- p$layers[c(1, 4, 2, 3)]
p



## Display table of outliers
outliers %>%
  mutate(
    Hour1 = hour(DateTime),
    Hour2 = hour(DateTime + 1800),
    Minute1 = minute(DateTime) %>% as.character(),
    Minute2 = minute(DateTime + 1800) %>% as.character(),
    Minute1 = if_else(Minute1 == "0", "00", Minute1),
    Minute2 = if_else(Minute2 == "0", "00", Minute2),
    Time = paste0(Hour1, ":", Minute1, " -- ", Hour2, ":", Minute2),
    Weekday = case_when(
      Weekday == "Thu" ~ "Thursday",
      Weekday == "Fri" ~ "Friday"
    ),
    Date = paste(day(Date), month(Date, label = TRUE, abbr = FALSE), year(Date)),
  ) %>%
  select(Date, Weekday, Time, res) %>%
  knitr::kable(
    format='latex',
    col.names = c("Date", "Day of week", "Time period", "Residual"),
    digits = 1, align = "rlcr", booktabs = TRUE, linesep=10,
    position="!bh",
    caption = "The ten residuals that are largest in absolute value after an STR decomposition."
  )
