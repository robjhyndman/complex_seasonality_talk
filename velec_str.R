# Fit stR model
# Assumes velec.R has already been run

library(stR)
library(distributional)

# Fit str with no covariates
if (file.exists("velec_str.rds")) {
  velec_str <- readRDS("velec_str.rds")
} else {
  velec_str <- AutoSTR(velec_msts, gapCV = 24 * 7)
  saveRDS(velec_str, "velec_str.rds")
}

# Fit str with covariates
if (file.exists("velec_str_x.rds")) {
  velec_str_x <- readRDS("velec_str_x.rds")
} else {
  # Set up predictors, starting with automatically chosen ones
  Predictors <- velec_str$input$predictors
  names(Predictors) <- c("Trend", "DSeason", "WSeason", "ASeason")
  # Annual seasonality
  Predictors$ASeason$times <- seq(NROW(velec_noly))
  Predictors$ASeason$timeKnots <- seq(from = 1, to = NROW(velec_noly), length.out = 6)
  Predictors$ASeason$lambdas <- c(4000, 400, 4000)
  # Weekly seasonality
  Predictors$WSeason$times <- seq(NROW(velec_noly))
  Predictors$WSeason$timeKnots <- seq(from = 1, to = NROW(velec_noly), length.out = 6)
  Predictors$WSeason$lambdas <- c(15, 0.1, 100)
  # Daily seasonality
  Predictors$DSeason$times <- seq(NROW(velec_noly))
  Predictors$DSeason$timeKnots <- seq(from = 1, to = NROW(velec_noly), length.out = 12)
  Predictors$DSeason$lambdas <- c(1.3e6, 0.01, 4e8)
  # Temperature
  Predictors$Temp <- Predictors$Trend
  Predictors$Temp$name <- "Temperature"
  Predictors$Temp$data <- velec_noly$Temperature
  Predictors$Temp$lambdas <- c(1e7, 0, 0)
  Predictors$Tempsq <- Predictors$Temp
  Predictors$Tempsq$name <- "Temperature^2"
  Predictors$Tempsq$data <- velec_noly$Temperature^2
  # Remove trend
  Predictors$Trend <- NULL
  # Lambdas
  lambdas <- list(DSeason = list(lambdas=c(15, 1, 100)),
                  WSeason = list(lambdas=c(15, 1, 100)),
                  ASeason = list(lambdas=c(2000, 500, 2000)),
                  Temp = list(lambdas=c(1e8, 0, 0)),
                  Tempsq = list(lambdas=c(1e8, 0, 0))
  )
  # STR decomposition of electricity data
  velec_str_x <- STR(
    data = velec_msts,
    predictors = Predictors,
    lambdas = lambdas,
    pattern = c(rep(TRUE,6),rep(FALSE,3),rep(FALSE,6)),
    gapCV = 24 * 7
  )
  saveRDS(velec_str_x, "velec_str_x.rds")
}
