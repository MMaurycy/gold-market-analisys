# --- 0. Install and load packages ---
# Installs packages if they are not already installed
install.packages(c("quantmod", "rugarch", "vars", "patchwork", "devtools", "dygraphs"))
install.packages("eventstudies")
# Loads required libraries
library(quantmod)
library(xts)
library(zoo)
library(readr)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyr)
library(tseries)
library(urca)
library(rugarch)
library(vars)
library(eventstudies)
library(ggplot2)
library(scales)
library(patchwork)
library(devtools)
library(dygraphs)

# Installs a specific version of the eventstudies package from GitHub
devtools::install_github("nipfpmf/eventstudies", ref="v1.2.2")

# --- 1. Script Settings ---
# Defines the start and end date for the analysis
start_date <- as.Date("2013-01-01")
end_date <- as.Date("2023-12-31")
# Defines the path to the data directory
data_directory <- "data/Part I"

# --- 2. Load FRED data from CSV files ---
# Function to load data from a CSV file (FRED format)
load_fred_csv <- function(filename, value_colname) {
  filepath <- file.path(data_directory, filename)
  df <- read_csv(filepath, show_col_types = FALSE) %>%
    mutate(
      Date = as.Date(observation_date),
      Value = as.numeric(.data[[value_colname]])
    ) %>%
    filter(!is.na(Date), !is.na(Value), Date >= start_date & Date <= end_date)
  xts_data <- xts(df$Value, order.by = df$Date)
  colnames(xts_data) <- value_colname
  return(xts_data)
}

# Creates an empty list to store data
list_data <- list()

# Loads individual FRED data series using the function
list_data$VIXCLS <- load_fred_csv("VIXCLS.csv", "VIXCLS")
list_data$DCOILWTICO <- load_fred_csv("DCOILWTICO.csv", "DCOILWTICO")
list_data$DTWEXBGS <- load_fred_csv("DTWEXBGS.csv", "DTWEXBGS")
list_data$DFF <- load_fred_csv("DFF.csv", "DFF")
list_data$CPIAUCSL <- load_fred_csv("CPIAUCSL.csv", "CPIAUCSL")
list_data$CP0000EZ19M086NEST <- load_fred_csv("CP0000EZ19M086NEST.csv", "CP0000EZ19M086NEST")
list_data$USEPUINDXD <- load_fred_csv("USEPUINDXD.csv", "USEPUINDXD")

# --- 3. Load SPOT data (Investing.com format) ---
# Function to load spot price data from CSV (Investing.com format)
load_spot_data <- function(filename, symbol_name) {
  filepath <- file.path(data_directory, filename)
  df <- read_csv(filepath, show_col_types = FALSE) %>%
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Value = as.numeric(gsub(",", "", Price)) # Removes commas from prices
    ) %>%
    filter(!is.na(Date), !is.na(Value), Date >= start_date & Date <= end_date)
  xts_data <- xts(df$Value, order.by = df$Date)
  colnames(xts_data) <- symbol_name
  return(xts_data)
}

# Loads spot price data for gold and silver
list_data$Gold_Spot <- load_spot_data("XAUUSD.csv", "Gold_Spot")
list_data$Silver_Spot <- load_spot_data("XAGUSD.csv", "Silver_Spot")

# --- 4. Load GPR data from XLS file (with error handling) ---
# Defines the path to the GPR file
gpr_file_path <- file.path(data_directory, "data_gpr_daily_recent.xls")

# Checks if the GPR file exists
if (file.exists(gpr_file_path)) {
  tryCatch({ # Tries to read and process the Excel file
    gpr_raw <- read_excel(gpr_file_path, col_names = TRUE)

    gpr_data <- gpr_raw %>%
      transmute(
        Date = as.Date(date, format = "%d.%m.%Y"), # Converts date
        GPR = as.numeric(GPRD) # Converts GPR value to numeric
      ) %>%
      filter(!is.na(Date), !is.na(GPR), Date >= start_date & Date <= end_date) %>% # Filters data
      arrange(Date) # Sorts by date

    gpr_xts <- xts(gpr_data$GPR, order.by = gpr_data$Date) # Creates an xts object
    colnames(gpr_xts) <- "GPR"
    list_data$GPR <- gpr_xts # Adds GPR to the data list

    cat("GPR data successfully loaded from XLS.\n") # Success message

  }, error = function(e) { # Error handling for reading
    cat("Error reading GPR Excel file:", conditionMessage(e), "\n")
  })
} else { # Message if file does not exist
  cat("GPR file not found:", gpr_file_path, "\n")
}

# --- 5. Merge all data into one xts object ---
# Merges all xts objects from the `list_data`
merged_data <- Reduce(function(...) merge.xts(..., join = "outer"), list_data)

# --- Download S&P 500 (^GSPC) data from Yahoo Finance ---
cat("\n--- GETTING S&P 500 from Yahoo (CAPM Market) ---\n")
tryCatch({ # Tries to download S&P 500 data
  # Use auto.assign=FALSE => result in sp500_data object
  sp500_data <- getSymbols(
    Symbols = "^GSPC",
    from    = start_date,
    to      = end_date,
    auto.assign = FALSE # Result is returned, not assigned globally
  )
  # sp500_data is an xts object
  sp500_close <- Ad(sp500_data)          # Gets the adjusted closing price
  sp500_ret   <- diff(log(sp500_close))  # Calculates logarithmic returns
  colnames(sp500_ret) <- "rSP500" # Assigns a column name

  # Merges S&P 500 returns with the rest of the data
  merged_data <- merge(merged_data, sp500_ret, join = "outer")
}, error=function(e){ # Error handling for download
  cat("Failed to getSymbols for ^GSPC:", conditionMessage(e), "\n")
})

# --- Re-filter date range after adding S&P 500 ---
# Ensures all data is within the specified date range
merged_data <- merged_data[index(merged_data) >= start_date & index(merged_data) <= end_date]

# --- 6. Fill missing data (na.locf method) ---
# Uses "last observation carried forward" method to fill NAs
if ("GPR" %in% colnames(merged_data)) merged_data$GPR <- na.locf(merged_data$GPR)
if ("USEPUINDXD" %in% colnames(merged_data)) merged_data$EPU_US <- na.locf(merged_data$USEPUINDXD) # Renames for EPU
if ("DFF" %in% colnames(merged_data)) merged_data$FedRate <- na.locf(merged_data$DFF) # Renames for Fed Rate
if ("CPIAUCSL" %in% colnames(merged_data)) merged_data$CPI_US_D <- na.locf(merged_data$CPIAUCSL) # Renames for US CPI
if ("CP0000EZ19M086NEST" %in% colnames(merged_data)) merged_data$HICP_EU_D <- na.locf(merged_data$CP0000EZ19M086NEST) # Renames for EU HICP

# --- 7. Preview data structure ---
# Displays the first few rows and a summary of the data
print(head(merged_data))
print(summary(merged_data))

# --- 8. Calculate logarithmic returns (prices) ---
# Defines price variables
price_vars <- c("Gold_Spot", "Silver_Spot")
# Loop to calculate log-returns for price variables
for (var in price_vars) {
  if (var %in% colnames(merged_data)) {
    price_series <- merged_data[, var]
    log_ret <- diff(log(price_series)) # Calculates log(Pt / Pt-1)
    colnames(log_ret) <- paste0("r", var) # Assigns name 'r' + variable name
    merged_data <- merge(merged_data, log_ret) # Adds to the dataset
  }
}

# Note: rSP500 is already calculated, do not recalculate.

# --- 9. Difference non-price series (levels) ---
# Defines variables at levels (non-price)
level_vars <- c("GPR", "EPU_US", "VIXCLS", "FedRate") # Inflation removed, often used as % y/y change
# Loop to difference non-price variables (if they have enough data)
for (var in level_vars) {
  if (var %in% colnames(merged_data)) {
    series <- na.omit(merged_data[, var]) # Removes NA before differencing
    if (length(series) > 30) { # Minimum series length condition
      d_series <- diff(series) # Calculates the first difference (Xt - Xt-1)
      colnames(d_series) <- paste0("d", var) # Assigns name 'd' + variable name
      merged_data <- merge(merged_data, d_series) # Adds to the dataset
    }
  }
}

# --- 10. Prepare final data for analysis ---
# Selects only calculated returns ('r'), differences ('d'), and rSP500
analysis_data <- merged_data[, grep("^(r|d|rSP500)", colnames(merged_data), value = TRUE)]
# Removes rows with any NA values (important for models)
analysis_data <- na.omit(analysis_data)

# --- 11. Stationarity Tests (ADF and KPSS) ---
cat("\nADF and KPSS Test Results:\n")
# Loop checking stationarity of each series in `analysis_data`
for (col in colnames(analysis_data)) {
  series <- analysis_data[, col]
  # ADF Test (null hypothesis: non-stationarity)
  adf_result <- tryCatch(adf.test(series), error = function(e) NA)
  adf <- ifelse(is.list(adf_result), adf_result$p.value, NA) # Gets the p-value
  # KPSS Test (null hypothesis: stationarity)
  kpss <- if (length(na.omit(series)) > 20) { # Requires minimum length
    tryCatch({
      test <- ur.kpss(as.numeric(series), type = "mu") # Test version with level ('mu')
      test@teststat > test@cval["5pct"] # True if we reject H0 (i.e., it is non-stationary according to KPSS)
    }, error = function(e) NA)
  } else {
    NA
  }
  # Prints results
  cat(sprintf("%s: ADF p=%.3f | KPSS non-stationary=%s\n", col, adf, kpss))
}

# --- 12. Data ready for modeling (VAR/GARCH/Event Study) ---
cat("\nData prepared for modeling VAR, GARCH, and Event Study.\n")

# --- 13. Vector Autoregression (VAR) Model ---
cat("\n--- VAR MODELING ---\n")
# Defines variables for the VAR model
var_vars <- c("rGold_Spot", "dGPR", "dVIXCLS", "dEPU_US")
# Checks which of these variables are available in the data
avail_vars <- intersect(var_vars, colnames(analysis_data))

# Checks if there are at least 2 variables for the VAR model
if (length(avail_vars) >= 2) {
  var_data <- na.omit(analysis_data[, avail_vars]) # Selects data and removes NAs
  # Selects the optimal lag for VAR (AIC criterion)
  lag_select <- VARselect(var_data, lag.max = 10, type = "const")
  best_lag <- lag_select$selection["AIC(n)"]
  cat("Selected lag (AIC):", best_lag, "\n")

  # Estimates the VAR model with the selected lag
  var_model <- VAR(var_data, p = best_lag, type = "const")
  summary_model <- summary(var_model) # Generates a model summary
  print(summary_model) # Displays the summary

  # Impulse Response Function (IRF)
  # Shows the response of rGold_Spot to a shock in dGPR over 20 periods
  irf_result <- irf(var_model, impulse = "dGPR", response = "rGold_Spot", n.ahead = 20, boot = TRUE)
  # Save IRF plot to PNG file
  png("irf_dGPR_rGold_Spot.png", width = 800, height = 600)
  plot(irf_result)
  dev.off() # Closes the file device
  cat("IRF plot saved as 'irf_dGPR_rGold_Spot.png'\n")


  # Forecast Error Variance Decomposition (FEVD)
  # Shows the contribution of each variable to the forecast error variance
  fevd_result <- fevd(var_model, n.ahead = 10)
  print(fevd_result) # Displays FEVD results
} else {
  cat("Not enough variables for VAR model.\n")
}

# --- 14. GARCH Model (with ARMA(1,1) in mean equation and regressors) ---
cat("\n--- GARCH MODELING (ARMA(1,1) in mean) ---\n")
# Checks if gold returns exist
if ("rGold_Spot" %in% colnames(analysis_data)) {
  # Defines external regressors for GARCH (affecting variance)
  regressors <- c("dGPR", "dVIXCLS", "dEPU_US")
  # Selects available regressors and creates a matrix
  ext_regs <- analysis_data[, intersect(regressors, colnames(analysis_data))]
  ext_regs_mat <- as.matrix(ext_regs)

  # Defines the GARCH model specification:
  spec_arma <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = ext_regs_mat), # sGARCH(1,1) with regressors
    mean.model = list(armaOrder = c(1, 1), include.mean = TRUE), # ARMA(1,1) in the mean equation
    distribution.model = "std" # Student's t-distribution for residuals
)

# Estimates the GARCH model
fit_sGARCH <- ugarchfit(spec_arma, data = analysis_data$rGold_Spot)
print(fit_sGARCH) # Displays estimation results

# Save GARCH residuals plot to PNG file
png("garch_residuals_rGold_Spot.png", width = 800, height = 600)
plot(fit_sGARCH, which = 2) # Standardized residuals plot
dev.off()
cat("GARCH residuals plot saved as 'garch_residuals_rGold_Spot.png'\n")
} else {
  cat("rGold_Spot not found for GARCH modeling.\n")
}

# --- 15. GARCH Model Diagnostics (plots) ---
# Checks if the GARCH model was estimated correctly
if (exists("fit_sGARCH")) {
  # Saves a set of GARCH diagnostic plots to a PNG file
  png("garch_diagnostics.png", width = 1200, height = 900)
  par(mfrow = c(2, 2)) # 2x2 plot layout
  plot(fit_sGARCH, which = 1) # Series with conditional standard deviation
  plot(fit_sGARCH, which = 2) # Standardized residuals series
  plot(fit_sGARCH, which = 3) # QQ plot of residuals
  plot(fit_sGARCH, which = 9) # ACF of standardized residuals
  dev.off()
  cat("GARCH diagnostics saved to 'garch_diagnostics.png'\n")
}

# --- A) GARCH(1,1) Model with ARMA(1,1) without external regressors ---
# Checks if gold returns exist
if ("rGold_Spot" %in% colnames(analysis_data)) {
  cat("\n--- Baseline sGARCH(1,1) Model WITHOUT regressors (ARMA(1,1)) ---\n")
  # Defines the GARCH model specification without external regressors
  spec_no_reg <- ugarchspec(
    variance.model = list(
      model = "sGARCH",
      garchOrder = c(1,1) # Just GARCH(1,1)
    ),
    mean.model = list(
      armaOrder = c(1,1), # ARMA(1,1) in the mean equation
      include.mean = TRUE
    ),
    distribution.model = "std" # Student's t-distribution
  )
  
  # Tries to estimate the baseline model
  fit_no_reg <- tryCatch({
    ugarchfit(spec = spec_no_reg, data = analysis_data$rGold_Spot)
  }, error = function(e) NULL) # Returns NULL in case of error
  
  # If estimation succeeded, display results
  if (!is.null(fit_no_reg)) {
    cat("\nBaseline sGARCH(1,1) with ARMA(1,1) (no vxreg):\n")
    show(fit_no_reg)
    assign("fit_no_reg", fit_no_reg, envir = .GlobalEnv) # Saves the model globally
  } else {
    cat("Could not fit GARCH no-reg model.\n")
  }
}

# --- B) Interactive GARCH visualization (dygraphs) ---
# Checks if the GARCH model with regressors exists
if (exists("fit_sGARCH")) {
  cat("\n--- Interactive Visualization with dygraphs (sGARCH + ARMA(1,1) & regressors) ---\n")
  # Gets conditional variance and standard deviation from the model
  cond_var <- fit_sGARCH@fit$var
  cond_sd  <- sqrt(cond_var)
  
  # Convert time index for conditional variance (from rownames)
  var_index <- as.Date(names(cond_var))
  # Checks if the time index is valid
  if (length(cond_sd) == length(var_index) && !anyNA(var_index)) {
    # Creates xts objects for conditional SD and returns
    cond_sd_xts <- xts(cond_sd, order.by = var_index)
    returns_xts <- xts(analysis_data$rGold_Spot, order.by = index(analysis_data))
    # Finds the common date range
    common_index <- base::intersect(as.Date(index(returns_xts)), as.Date(index(cond_sd_xts)))
    
    # If there are common dates, create data for the plot
    if (length(common_index) > 0) {
      plot_data <- merge(returns_xts[common_index], cond_sd_xts[common_index])
      colnames(plot_data) <- c("rGold_Spot", "Cond_SD") # Assigns column names
      
      # Instruction on how to generate the interactive plot in RStudio
      cat("To view interactive plot, run in RStudio:\n")
      cat("dygraph(plot_data) %>% dySeries('rGold_Spot', axis='y') %>% dySeries('Cond_SD', axis='y2')\n")
    } else {
      cat("No overlapping dates to plot interactive dygraph.\n")
    }
  } else {
    cat("No valid time-based index for GARCH variance. Skipping dygraph.\n")
  }
}

# --- C) GARCH Forecast (e.g., 5 days ahead) ---
# Checks if the GARCH model with regressors exists
if (exists("fit_sGARCH")) {
  cat("\n--- Forecasting volatility 5 days ahead (sGARCH + ARMA(1,1) & regressors) ---\n")
  # Generates a forecast for 5 periods ahead
  fc <- ugarchforecast(fit_sGARCH, n.ahead = 5)
  show(fc) # Displays the forecast
  # Saves the conditional variance forecast plot to a file
  png("garch_forecast.png", width = 900, height = 600)
  # 'which = 2' => conditional variance forecast.
  res_plot <- tryCatch({ # Tries to plot the variance forecast
    plot(fc, which = 2)
  }, error=function(e) { # If it fails (e.g., rolling mode error), plots the mean forecast
    cat("Plot with which=2 failed (rolling error). Plotting which=1 (mean forecast).\n")
    plot(fc, which=1)
  })
  dev.off() # Closes the device
  cat("GARCH forecast plot saved as 'garch_forecast.png'\n")
}

################################################################################
# --- 16. Event Study - XTS Approach ---
################################################################################

cat("\n--- EVENT STUDY EXAMPLE (xts approach) ---\n")

# Checks if gold returns are available
if ("rGold_Spot" %in% colnames(analysis_data)) {
  
  # 1. Prepare an xts object with returns (rGold_Spot) and date index
  z_xts <- xts(analysis_data$rGold_Spot, order.by = index(analysis_data))
  colnames(z_xts) <- "Gold_Spot" # Column name must match 'unit' in the event table
  
  # 2. Create an event table (key dates 2013-2023)
  #    (key economic, geopolitical events)
  event_table <- data.frame(
    unit = "Gold_Spot",  # Data series name (must match column name in z_xts)
    when = as.Date(c( # Event dates
      "2013-03-15",  # Cyprus Crisis
      "2013-05-22",  # Taper tantrum (Fed hints at QE reduction)
      "2014-03-18",  # Annexation of Crimea by Russia
      "2015-06-29",  # Greek Crisis (referendum, capital controls)
      "2016-06-23",  # Brexit Referendum
      "2016-11-08",  # US Presidential Election (Trump)
      "2017-04-23",  # French Presidential Election (Round 1, Macron vs Le Pen)
      "2018-03-22",  # Start of US-China trade war (tariffs on steel/aluminum)
      "2019-07-31",  # First Fed rate cut since 2008 crisis
      "2020-03-11",  # WHO declares COVID-19 a pandemic
      "2020-04-20",  # WTI oil price drops below zero
      "2021-05-12",  # US inflation spike (CPI above expectations)
      "2022-02-24",  # Russia invades Ukraine
      "2022-08-29",  # EU energy crisis (gas prices)
      "2023-03-10"   # Silicon Valley Bank (SVB) collapse
    )),
    name = c( # Event names (for identification)
      "CyprusCrisis",
      "TaperTantrum",
      "Crimea",
      "GreekCrisis",
      "Brexit",
      "USElection2016",
      "FRElection2017",
      "TradeWar2018",
      "FedCut2019",
      "Covid",
      "WTInegative",
      "Inflation2021",
      "UkraineInvasion",
      "EnergyCrisisEU",
      "SVBCollapse"
    ),
    stringsAsFactors = FALSE
  )
  
  # 3. Use phys2eventtime from eventstudies package (±5 day window)
  #    Converts data from physical time to event time
  cat("\n--- Using phys2eventtime with width=5 (±5 days) ---\n")
  es_data <- phys2eventtime(z = z_xts, # Input data (returns)
                            events = event_table, # Event table
                            width  = 5) # Event window width (5 days before and 5 days after)
  
  # Checks if the result contains data in the event window
  if (is.null(es_data$z.e)) {
    cat("phys2eventtime produced NULL z.e => Possibly no matching dates for at least some events.\n")
  } else {
    # Calculate the average reaction for all events in the [-5..+5] window
    es_wide <- window(es_data$z.e, start=-5, end=5) # Selects data from the [-5, 5] window
    if (!is.null(es_wide) && ncol(es_wide) > 0) { # Checks if there is data
      # Calculates the average return for each day in the event window (average across columns, where each column is an event)
      mean_reaction <- rowMeans(es_wide, na.rm=TRUE)
      cat("\nAverage Reaction (return) across events in [-5..+5]:\n")
      print(mean_reaction)
      
      # To see the cumulative sum/mean for each event separately:
      # apply(es_wide, 2, function(x) sum(x, na.rm=TRUE)) # sum or mean
      # car_per_event <- colSums(es_wide, na.rm=TRUE) # Cumulative return for each event
      
    } else {
      cat("Window [-5, 5] is empty => no data around events?\n")
    }
  }
} else {
  cat("No rGold_Spot for event study.\n")
}


################################################################################
# --- 17. Extended Event Study (T-Test, Baseline Window, CAPM) ---
################################################################################

cat("\n--- EXTENDED EVENT STUDY (T-TEST, BASELINE, CAPM) ---\n")

# Checks if gold returns are available
if ("rGold_Spot" %in% colnames(analysis_data)) {
  
  cat("\n[1] Example: T-Test and baseline window for multiple events\n")
  
  # Event table (same as before, but in a different format)
  events_extended <- data.frame(
    eventDate = as.Date(c(
      "2013-03-15", "2013-05-22", "2014-03-18", "2015-06-29", "2016-06-23",
      "2016-11-08", "2017-04-23", "2018-03-22", "2019-07-31", "2020-03-11",
      "2020-04-20", "2021-05-12", "2022-02-24", "2022-08-29", "2023-03-10"
    )),
    eventName = c(
      "CyprusCrisis", "TaperTantrum", "Crimea", "GreekCrisis", "Brexit",
      "USElection2016", "FRElection2017", "TradeWar2018", "FedCut2019", "Covid",
      "WTInegative", "Inflation2021", "UkraineInvasion", "EnergyCrisisEU", "SVBCollapse"
    ),
    stringsAsFactors = FALSE
  )
  
  # Recreates the xts object with gold returns
  z_xts2 <- xts(analysis_data$rGold_Spot, order.by = index(analysis_data))
  colnames(z_xts2) <- "Gold_Spot"
  
  # Defines the length of the baseline window (before event) and event window (after event)
  window_before <- 100  # baseline window: 100 days before the event
  window_after  <- 5    # event window: event day + 5 days after (6 days total)
  
  # Loop processing each event
  for (i in seq_len(nrow(events_extended))) {
    evDate <- events_extended$eventDate[i] # Event date
    evName <- events_extended$eventName[i] # Event name
    
    # Defines start and end dates of the baseline window
    baseline_start <- evDate - window_before
    baseline_end   <- evDate - 1
    
    # Defines start and end dates of the event window
    event_start <- evDate
    event_end   <- evDate + window_after
    
    # Selects data for the baseline and event windows
    baseline_data <- z_xts2[paste0(baseline_start,"/",baseline_end)]
    event_data    <- z_xts2[paste0(event_start,"/",event_end)]
    
    # Checks if there is enough data in the baseline window
    if (nrow(baseline_data) < 10) {
      cat(sprintf("[%s] Not enough baseline data in [%s, %s]\n", evName, baseline_start, baseline_end))
      next # Moves to the next event
    }
    
    # Checks if there is enough data in the event window
    if (nrow(event_data) < 1) {
      cat(sprintf("[%s] Not enough event data in [%s, %s]\n", evName, event_start, event_end))
      next # Moves to the next event
    }
    
    # Calculates the average return in the baseline window (expected return)
    baseline_avg <- mean(baseline_data$Gold_Spot, na.rm=TRUE)
    # Calculates abnormal returns (AR) in the event window: Actual - Expected
    abn_ret <- as.numeric(event_data$Gold_Spot) - baseline_avg
    # Calculates cumulative abnormal return (CAR) by summing AR
    car <- sum(abn_ret)
    
    # Displays results for the given event
    cat(sprintf("\n=== %s (%s) ===\n", evName, as.character(evDate)))
    cat(sprintf("Baseline avg ret = %.4f (window: %s to %s, n=%d)\n",
                baseline_avg, baseline_start, baseline_end, nrow(baseline_data)))
    cat(sprintf("Event window = %s to %s, n=%d\n",
                event_start, event_end, nrow(event_data)))
    cat("Abnormal returns (AR) in event window:\n")
    print(abn_ret)
    cat(sprintf("CAR (sum of AR) = %.4f\n", car))
    
    # Performs a t-test for abnormal returns (H0: mean AR = 0)
    t_res <- t.test(abn_ret, mu=0)
    cat("T-test for abnormal returns != 0:\n")
    print(t_res)
    cat("\n")
  }
  
  cat("\n[2] CAPM approach to calculating abnormal returns\n")
  cat("    (We have rSP500 in analysis_data => CAPM can be used)\n\n")
  
  # Checks if S&P 500 returns are available
  if ("rSP500" %in% colnames(analysis_data)) {
    cat("=== CAPM-based abnormal returns example ===\n")
    
    # Step 1: Get gold and S&P 500 returns, align data
    gold_ret  <- as.numeric(analysis_data$rGold_Spot)
    sp500_ret <- as.numeric(analysis_data$rSP500)
    
    # Creates a data frame and removes missing values
    capm_data <- data.frame(gold=gold_ret, spx=sp500_ret)
    capm_data <- na.omit(capm_data)
    
    # Estimates the CAPM model: R_gold = alpha + beta * R_spx + epsilon
    capm_model <- lm(gold ~ spx, data=capm_data)
    cat("CAPM model summary (Gold vs SP500):\n")
    print(summary(capm_model))
    
    # Step 2: Abnormal returns = Actual gold return - Predicted return from CAPM model
    # Predicted return = alpha_hat + beta_hat * R_spx
    # Abnormal return (AR) = residuals from the CAPM model (epsilon_hat)
    pred <- predict(capm_model, newdata=capm_data)
    abn <- capm_data$gold - pred
    cat("First few abnormal returns (CAPM approach):\n")
    print(head(abn))
    
    # 3. Create an xts object for abnormal returns (AR) with the correct date index
    # Finds row indices where both series (gold and SP500) had values
    dateIndex <- index(analysis_data)[which(!is.na(analysis_data$rGold_Spot) & !is.na(analysis_data$rSP500))]
    abn_xts   <- xts(abn, order.by=dateIndex) # Creates xts with AR and dates
    
    cat("\nObject 'abn_xts' contains abnormal returns calculated via CAPM.\n")
    cat("Can be used in event studies (e.g., in the loop above instead of z_xts2) to examine AR reactions.\n")
    
  } else {
    cat("No 'rSP500' in analysis_data => skipping CAPM example.\n")
  }
  
} else {
  cat("No rGold_Spot for extended event study.\n")
}


################################################################################
# --- 18. Robustness Checks ---
################################################################################

cat("\n--- ROBUSTNESS CHECKS ---\n")

# Checks if gold returns are available
if ("rGold_Spot" %in% colnames(analysis_data)) {
  
  # Recreates xts for gold
  z_gold <- xts(analysis_data$rGold_Spot, order.by = index(analysis_data))
  colnames(z_gold) <- "Gold_Spot"
  
  # Selects a few key events for testing
  events_list <- as.Date(c(
    "2016-06-23",  # Brexit
    "2020-03-11",  # Covid
    "2022-02-24"   # Ukraine Invasion
  ))
  
  # Defines different event window lengths to test
  window_lengths <- c(3, 5, 10, 15)  # different event window lengths (e.g., ±3, ±5 days)
  # Defines different baseline window lengths (although here we use a constant 100)
  # baseline_lengths <- c(60, 100, 120)
  
  # Loop testing different event window lengths for selected events
  for (win_len in window_lengths) {
    for (evDate in events_list) {
      event_name <- paste0("Event_", evDate, "_Win", win_len)
      cat(sprintf("\n[%s]\n", event_name))
      
      # Selects data for the event window (symmetrically around the date)
      event_window <- z_gold[paste0(format(evDate - win_len), "/", format(evDate + win_len))]
      # Selects data for the baseline window (constant 100 days before)
      baseline_window <- z_gold[paste0(format(evDate - 100), "/", format(evDate - 1))]
      
      # Checks if there is enough data
      if (nrow(event_window) < 3 || nrow(baseline_window) < 30) {
        cat("Insufficient number of observations.\n")
        next
      }
      
      # Calculates baseline average and abnormal returns
      baseline_avg <- mean(baseline_window$Gold_Spot)
      abn_ret <- as.numeric(event_window$Gold_Spot) - baseline_avg
      
      # Displays results for this combination of event and window length
      cat(sprintf("Event window [%s to %s] (n=%d)\n",
                  index(event_window)[1], index(event_window)[nrow(event_window)], nrow(event_window)))
      cat(sprintf("CAR = %.4f\n", sum(abn_ret))) # Cumulative abnormal return
      ttest_res <- t.test(abn_ret, mu=0) # t-test for AR
      print(ttest_res)
    }
  }
  
  # Comparison for Silver (as an example of an additional test)
  if ("rSilver_Spot" %in% colnames(analysis_data)) {
    cat("\n[Silver Spot] Brexit Event Check (±5d)\n")
    # Creates xts for silver
    z_silver <- xts(analysis_data$rSilver_Spot, order.by = index(analysis_data))
    colnames(z_silver) <- "rSilver_Spot" # Assigns column name
    ev <- as.Date("2016-06-23") # Brexit date
    # Selects baseline and event windows for silver
    base <- z_silver[paste0(format(ev - 100), "/", format(ev - 1))]
    evt <- z_silver[paste0(format(ev - 5), "/", format(ev + 5))]
    
    # If there is enough data, calculates AR and CAR for silver
    if (nrow(base) >= 30 && nrow(evt) >= 3) {
      mu <- mean(base$`rSilver_Spot`) # Baseline average
      abn <- as.numeric(evt$`rSilver_Spot`) - mu # Silver abnormal returns
      cat("Silver abnormal returns around Brexit:\n")
      print(abn)
      cat("CAR for silver = ", sum(abn), "\n")
      print(t.test(abn, mu=0)) # t-test for silver AR
    }
  }
  
  # Robustness test using abnormal returns from the CAPM model
  cat("\n--- CAPM-based Event Study (Robustness) ---\n")
  
  # Checks if the `abn_xts` object exists (created in section 17)
  if (exists("abn_xts")) {
    # Loop over the same selected events
    for (evDate in events_list) {
      event_name <- paste0("CAPM_AR_", evDate, "_Win5") # Test name
      # Selects abnormal returns (from CAPM) in the ±5 day window
      event_window <- abn_xts[paste0(format(evDate - 5), "/", format(evDate + 5))]
      
      # Checks if there is enough data
      if (nrow(event_window) < 3) {
        cat(sprintf("[%s] Too few data points in event window.\n", event_name))
        next
      }
      
      # Displays analysis results for AR from CAPM
      cat(sprintf("\n[%s]\n", event_name))
      cat(sprintf("Event window: %s to %s (n=%d)\n",
                  index(event_window)[1], index(event_window)[nrow(event_window)], nrow(event_window)))
      ar_vals <- as.numeric(event_window) # AR values
      cat("CAR (from CAPM AR) =", round(sum(ar_vals), 5), "\n") # Cumulative AR
      print(t.test(ar_vals, mu = 0)) # t-test for AR from CAPM
    }
  } else {
    cat("Object `abn_xts` (CAPM-based abnormal returns) not found. Skipping this robustness test.\n")
  }
  
} else {
  cat("rGold_Spot not found for robustness checks.\n")
}

################################################################################
# --- 19. ACF/PACF Plots for Gold Returns ---
################################################################################

# Checks if gold returns are available
if ("rGold_Spot" %in% colnames(analysis_data)) {
  # Selects gold returns and removes any NAs (although analysis_data should already be clean)
  gold_returns <- na.omit(analysis_data$rGold_Spot)
  
  # Tries to generate and save ACF and PACF plots
  tryCatch({
    # Saves plots to PNG file
    png("acf_pacf_gold_returns.png", width = 1000, height = 600)
    par(mfrow = c(1, 2)) # 1x2 plot layout
    # Generates ACF (autocorrelation) plot
    acf(gold_returns, main = "ACF of Logarithmic Gold Returns")
    # Generates PACF (partial autocorrelation) plot
    pacf(gold_returns, main = "PACF of Logarithmic Gold Returns")
    dev.off() # Closes the file device
    cat("ACF/PACF plot saved to file 'acf_pacf_gold_returns.png'\n")
  }, error = function(e) { # Error handling for plot generation
    cat("Error creating ACF/PACF plot:", conditionMessage(e), "\n")
  })
} else {
  cat("rGold_Spot not found in analysis_data. Skipping ACF/PACF plot.\n")
}

# Displays the current working directory
getwd()