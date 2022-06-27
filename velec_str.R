# Fit stR model
# Assumes velec.R has already been run

library(stR)
library(distributional)

# Fit str with no covariates
if(file.exists("velec_str.rds")) {
  velec_str <- readRDS("velec_str.rds")
} else {
  velec_str <- AutoSTR(velec_msts, gapCV = 24 * 7)
  saveRDS(velec_str, "velec_str.rds")
}

# Fit str with covariates
if(file.exists("velec_str_x.rds")) {
  velec_str_x <- readRDS("velec_str_x.rds")
} else {
  # Set up predictors
  Predictors <- list()
  Predictors$Trend <- list(
    name = "Trend",
    data = rep(1, NROW(velec)),
    times = seq(NROW(velec)),
    seasons = rep(1, NROW(velec)),
    timeKnots = seq(from = 1, to = NROW(velec), length.out = 58),
    seasonalStructure = list(
      segments = list(c(0, 1)),
      sKnots = list(c(1, 0))
    ),
    lambdas = c(1500, 0, 0)
  )
  Predictors$ASeason <- list(
    name = "Annual seas",
    data = rep(1, NROW(velec)),
    times = seq(NROW(velec)),
    seasons = velec$AnnualSeasonality,
    timeKnots = seq(from = 1, to = NROW(velec), length.out = 6),
    seasonalStructure = list(
      segments = list(c(0, 8760)),
      sKnots = c(as.list(seq(24, 8760, 24)), list(c(8760, 0)))
    ),
    lambdas = c(1000, 1000, 1000)
  )
  Predictors$WSeason <- list(
    name = "Weekly seas",
    data = rep(1, NROW(velec)),
    times = seq(NROW(velec)),
    seasons = velec$WeeklySeasonality,
    timeKnots = seq(from = 1, to = NROW(velec), length.out = 6),
    seasonalStructure = list(
      segments = list(c(0, 168)),
      sKnots = c(as.list(seq(4, 166, 4)), list(c(168, 0)))
    ),
    lambdas = c(0.8, 0.6, 100)
  )
  Predictors$WDSeason <- list(
    name = "Daily seas",
    data = rep(1, NROW(velec)),
    times = seq(NROW(velec)),
    seasons = velec$WDSeasonality,
    timeKnots = seq(from = 1, to = NROW(velec), length.out = 12),
    seasonalStructure = list(
      segments = list(c(0, 24), c(100, 124)),
      sKnots = c(as.list(c(1:23, 101:123)), list(c(0, 24, 100, 124)))
    ),
    lambdas = c(0.003, 0, 240)
  )
  Predictors$TrendTempM <- list(
    name = "Trend temp",
    data = velec$Temperature,
    times = seq(NROW(velec)),
    seasons = rep(1, NROW(velec)),
    timeKnots = Predictors$Trend$timeKnots,
    seasonalStructure = Predictors$Trend$seasonalStructure,
    lambdas = c(1e7, 0, 0)
  )
  Predictors$TrendTempM2 <- list(
    name = "Trend temp^2",
    data = velec$Temperature^2,
    times = seq(NROW(velec)),
    seasons = rep(1, NROW(velec)),
    timeKnots = Predictors$Trend$timeKnots,
    seasonalStructure = Predictors$Trend$seasonalStructure,
    lambdas = c(1e7, 0, 0)
  )

  # STR decomposition of electricity data
  velec_str_x <- STR(
    data = velec_msts,
    predictors = Predictors,
    confidence = 0.95, gapCV = 24 * 7
  )
  saveRDS(velec_str_x, "velec_str_x.rds")
}
