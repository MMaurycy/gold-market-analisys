# --- 0. SETUP: Load necessary packages ---
packages <- c(
  "quantmod", "xts", "zoo", "readr", "dplyr", "lubridate", "readxl", "tidyr",
  "tseries", "urca", "rugarch", "vars", "ggplot2", "scales", "patchwork"
)
# Define required packages
packages <- c(
  "quantmod", "xts", "zoo", "readr", "dplyr", "lubridate", "readxl", "tidyr",
  "tseries", "urca", "rugarch", "vars", "svars", "ggplot2", "scales", "patchwork",
  "sandwich", "lmtest", "gridExtra", "car"
)
# Get currently installed packages
installed <- rownames(installed.packages())
# Loop through packages, install if missing, then load
for (p in packages) {
  if (!(p %in% installed)) install.packages(p)
  library(p, character.only = TRUE)
}

# --- 1. SETTINGS ---
start_date <- as.Date("2023-01-01")
end_date   <- as.Date("2025-04-15")
data_directory <- "data/Part II"

# --- 2. FUNCTION: Load CSV from FRED ---
load_fred_csv <- function(filename, colname) {
  filepath <- file.path(data_dir, filename)
  df <- read_csv(filepath, show_col_types = FALSE) %>%
    mutate(
      Date = as.Date(observation_date),
      Value = as.numeric(.data[[colname]])
    ) %>%
    filter(!is.na(Date), !is.na(Value), Date >= start_date, Date <= end_date)
  
  xts_obj <- xts(df$Value, order.by = df$Date)
  colnames(xts_obj) <- colname
  return(xts_obj)
}

# --- 3. FUNCTION: Load CSV from Investing (fixed parser) ---
load_investing_csv <- function(filename, symbol_name) {
  filepath <- file.path(data_dir, filename)
  
  # Check if file exists
  if (!file.exists(filepath)) {
    stop(paste("File not found:", filepath)) # Error message if file is missing
  }
  
  # Read CSV, skipping the header row and defining column names
  df <- read_csv(filepath, skip = 1, col_names = c("Date", "Price", "Open", "High", "Low", "Vol", "Change"), show_col_types = FALSE)
  # Convert Date column
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  # Order by date (important for time series)
  df <- df[order(df$Date), ]
  # Convert to xts, keeping relevant columns
  df_xts <- xts(df[, -1], order.by = df$Date) # Exclude Date column from data part
  # Assign column names with symbol prefix
  colnames(df_xts) <- paste0(symbol_name, "_", colnames(df_xts))
  return(df_xts)
}


# --- 4. LOAD ALL DATA ---
list_data <- list()

# FRED SOURCES (preferred if available)
list_data$VIX     <- load_fred_csv("VIXCLS.csv", "VIXCLS")
list_data$WTI     <- load_fred_csv("DCOILWTICO.csv", "DCOILWTICO")
list_data$FedRate <- load_fred_csv("DFF.csv", "DFF")
list_data$Bond2Y  <- load_fred_csv("DGS2.csv", "DGS2")
list_data$DXY     <- load_fred_csv("DTWEXBGS.csv", "DTWEXBGS")
list_data$EPU     <- load_fred_csv("USEPUINDXD.csv", "USEPUINDXD") # Economic Policy Uncertainty Index

# INVESTING SOURCES (if no FRED equivalent or for comparison)
list_data$SP500 <- load_investing_csv("S&P 500.csv", "SP500")
list_data$Gold  <- load_investing_csv("XAU_USD.csv", "Gold")

# --- 5. MERGE ALL INTO ONE XTS OBJECT ---
# Combine all xts objects in the list using an outer join
merged_data <- Reduce(function(x, y) merge(x, y, join = "outer"), list_data)
# Filter the merged data to the specified date range
merged_data <- merged_data[index(merged_data) >= start_date & index(merged_data) <= end_date]
# Fill missing values using the last observation carried forward method
merged_data <- na.locf(merged_data, na.rm = FALSE) # na.rm=FALSE keeps leading NAs if any

# Show structure preview
print(head(merged_data))
print(summary(merged_data))

# --- 6. CATEGORICAL EPU DATA (monthly subcategories) ---
epu_cat_path <- file.path(data_dir, "Categorical_EPU_Data.xlsx")
# Read the Excel file
epu_cat_raw <- read_excel(epu_cat_path)

# Preview column names to ensure they exist
print(names(epu_cat_raw))

# Remove rows with missing Year or Month
epu_cat_raw <- epu_cat_raw %>% filter(!is.na(Year), !is.na(Month))

# Create date variable from Year and Month — assuming Day = 1
epu_cat_raw$Date <- make_date(epu_cat_raw$Year, epu_cat_raw$Month, 1)

# Manual assignment of relevant columns to new names
# Select specific columns based on their names (ensure these names match the Excel file)
epu_cat <- epu_cat_raw[, c("Date",
                           "9. Trade policy",
                           "10. Sovereign debt, currency crises",
                           "Fiscal Policy (Taxes OR Spending)", # Check this name carefully
                           "2. Monetary policy")]
# Assign simpler column names
colnames(epu_cat) <- c("Date", "EPU_Trade", "EPU_Debt", "EPU_Fiscal", "EPU_Monetary")

# Date filtering
epu_cat <- epu_cat[epu_cat$Date >= start_date & epu_cat$Date <= end_date, ]

# Convert to xts and carry forward daily
epu_cat_xts <- xts(epu_cat[, -1], order.by = epu_cat$Date) # Exclude Date column
epu_cat_xts <- na.locf(epu_cat_xts) # Carry forward monthly values to daily frequency
# Merge categorical EPU data with the main dataset
merged_data <- merge(merged_data, epu_cat_xts, join = "outer")

# --- GPR DAILY ---
gpr_path <- file.path(data_dir, "data_gpr_daily_recent.xls")
# Read the first sheet, assuming headers are present
gpr_raw <- read_excel(gpr_path, sheet = 1, col_names = TRUE)

# Check columns, especially the names of date and GPR columns
print(names(gpr_raw)[1:5]) # Print first 5 column names for verification

# Rename the first column to DAY if it's not already named that (assuming date info is YYYYMMDD format)
colnames(gpr_raw)[1] <- "DAY"

# Replace commas with dots and convert GPRD
# Ensure the GPR column is named 'GPRD'
if (!"GPRD" %in% names(gpr_raw)) {
  # Find the column likely containing GPR data (e.g., named "GPR") and rename it
  colnames(gpr_raw)[which(names(gpr_raw) == "GPR")] <- "GPRD" # Adjust if original name is different
}
# Convert GPRD column to numeric, replacing comma decimal separators
gpr_raw$GPRD <- as.numeric(gsub(",", ".", gpr_raw$GPRD))

# Create Date column and filter
gpr_raw$Date <- as.Date(as.character(gpr_raw$DAY), format = "%Y%m%d") # Convert YYYYMMDD to Date
# Select non-missing Date and GPRD, keeping only these two columns
gpr_daily <- gpr_raw[!is.na(gpr_raw$Date) & !is.na(gpr_raw$GPRD), c("Date", "GPRD")]
# Rename columns for clarity
colnames(gpr_daily) <- c("Date", "GPR")
# Filter by date range
gpr_daily <- gpr_daily[gpr_daily$Date >= start_date & gpr_daily$Date <= end_date, ]

# Convert GPR data to xts object
gpr_xts <- xts(gpr_daily$GPR, order.by = gpr_daily$Date)
colnames(gpr_xts) <- "GPR"
# Merge GPR data with the main dataset
merged_data <- merge(merged_data, gpr_xts, join = "outer")

# --- 7. DAILY LOG RETURNS FOR SELECTED SERIES ---
cat("\n--- Logarithmic returns (daily) ---\n")
# Define price variables (adjust based on loaded data, e.g., Gold_Price from Investing)
price_vars <- c("Gold_Price", "SP500_Price")
# Loop through price variables to calculate log returns
for (var in price_vars) {
  if (var %in% colnames(merged_data)) { # Check if the variable exists
    price_series <- merged_data[, var] # Extract the price series
    ret <- diff(log(price_series)) # Calculate log return: log(P_t / P_{t-1})
    colnames(ret) <- paste0("r", var) # Name the new column 'r' + original name
    merged_data <- merge(merged_data, ret) # Merge returns back into the main dataset
  }
}

# --- 8. ACF/PACF OF DAILY RETURNS ---
cat("\n--- ACF/PACF for daily returns ---\n")
# Define return variables to analyze
return_vars <- c("rGold_Price", "rSP500_Price")
# Loop through return variables
for (var in return_vars) {
  if (var %in% colnames(merged_data)) { # Check if variable exists
    series <- na.omit(merged_data[, var]) # Extract series and remove NAs
    tryCatch({ # Attempt to generate and save plots
      png(paste0("acf_pacf_", var, ".png"), width = 1000, height = 600) # Open PNG device
      par(mfrow = c(1, 2)) # Set plot layout to 1 row, 2 columns
      acf(series, main = paste("ACF for:", var)) # Plot ACF
      pacf(series, main = paste("PACF for:", var)) # Plot PACF
      dev.off() # Close PNG device
      cat(paste("ACF/PACF plot saved to file acf_pacf_", var, ".png\n", sep = "")) # Success message
    }, error = function(e) { # Error handling
      cat(paste("Error in ACF/PACF for:", var, ":", conditionMessage(e), "\n"))
    })
  }
}

# --- 9. STATIONARITY TESTS ---
cat("\n--- ADF and KPSS stationarity tests ---\n")
# Loop through return variables again for testing
for (var in return_vars) {
  if (var %in% colnames(merged_data)) {
    series <- na.omit(merged_data[, var]) # Extract series and remove NAs
    cat(paste("\n[", var, "]\n", sep = "")) # Print variable name being tested
    
    # Perform Augmented Dickey-Fuller (ADF) test (H0: non-stationary)
    adf_result <- adf.test(series)
    cat("ADF test:\n")
    print(adf_result)
    
    # Perform Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test (H0: stationary)
    kpss_result <- ur.kpss(series) # Using urca package version
    cat("\nKPSS test:\n")
    print(summary(kpss_result)) # Print summary which includes test statistic and critical values
  }
}

# --- 10. GARCH(1,1) MODEL FOR rGold_Price ---
cat("\n--- GARCH(1,1) modeling for rGold_Price ---\n")
if ("rGold_Price" %in% colnames(merged_data)) { # Check if gold returns exist
  series <- na.omit(merged_data$rGold_Price) # Extract series and remove NAs
  # Define GARCH(1,1) specification with Normal distribution
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), # Standard GARCH(1,1)
                     mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), # Simple mean model (constant)
                     distribution.model = "norm") # Normal distribution
  # Fit the GARCH model
  fit <- ugarchfit(spec = spec, data = series)
  print(fit) # Print model fit summary
  
  # Save diagnostic plots (first 4 standard plots)
  png("garch_residuals.png", width = 1000, height = 600)
  par(mfrow = c(2, 2)) # 2x2 layout
  for (i in 1:4) plot(fit, which = i) # Plot diagnostics 1 through 4
  dev.off()
  cat("GARCH diagnostic plots saved to file garch_residuals.png\n")
}

# --- 11. GARCH(1,1) for rGold_Price with t-distribution and comparison with S&P 500 ---
cat("\n--- GARCH(1,1) t-Student for rGold_Price and comparison with S&P500 ---\n")
# Loop through gold and S&P 500 returns
for (var in c("rGold_Price", "rSP500_Price")) {
  if (var %in% colnames(merged_data)) { # Check if variable exists
    series <- na.omit(merged_data[, var]) # Extract series, remove NAs
    # Define GARCH(1,1) specification with Student's t-distribution
    spec_t <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), # sGARCH(1,1) for variance
      mean.model     = list(armaOrder = c(0, 0)), # Constant mean model (ARMA(0,0))
      distribution.model = "std" # Student's t-distribution
    )
    # Fit the model
    fit_t <- ugarchfit(spec = spec_t, data = series)
    cat(paste0("\n--- GARCH(1,1) model results with t-Student for ", var, " ---\n"))
    print(fit_t) # Print fit summary
    
    # Create xts with sigma (conditional volatility) and merge with merged_data
    sigma_name <- paste0("sigma_", var) # Create name for the sigma column
    # Extract conditional standard deviation (sigma) and create xts object
    sigma_xts <- xts(sigma(fit_t), order.by = index(series)) # Use index of the original series
    colnames(sigma_xts) <- sigma_name # Assign name
    merged_data <- merge(merged_data, sigma_xts, join = "outer") # Merge sigma back
  }
}

# --- 12. GARCH-X model for rGold_Price with GPR as external regressor ---
cat("\n--- GARCH-X model for rGold_Price with GPR ---\n")
# Check if required variables exist
if ("rGold_Price" %in% colnames(merged_data) && "GPR" %in% colnames(merged_data)) {
  # Align gold returns and GPR data, removing NAs introduced by merging/differencing
  gpr_aligned <- na.omit(merge(merged_data$rGold_Price, merged_data$GPR))
  # Define GARCH-X specification (GPR in mean equation)
  spec_x <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), # sGARCH(1,1) variance
    mean.model     = list(armaOrder = c(0, 0), external.regressors = matrix(gpr_aligned[, 2], ncol = 1)), # GPR as regressor in mean
    distribution.model = "std" # Student's t-distribution
  )
  # Fit the GARCH-X model
  fit_x <- ugarchfit(spec = spec_x, data = gpr_aligned[, 1]) # Use gold returns as dependent variable
  print(fit_x) # Print fit summary
  # Added: conditional variance from GARCHX
  # Extract sigma and align it properly before merging
  sigma_garchx_gold <- xts(sigma(fit_x), order.by = index(gpr_aligned))
  colnames(sigma_garchx_gold) <- "sigma_GARCHX_Gold"
  merged_data <- merge(merged_data, sigma_garchx_gold, join = "outer")
  
}

# --- B. CORRELATIONS OF CONDITIONAL VARIANCES ---
cat("\n--- Interdependence correlations ---\n")
# Calculate correlation between Gold volatility (simple GARCH) and GPR index
if (all(c("sigma_rGold_Price", "GPR") %in% colnames(merged_data))) {
  # Align the series and remove NAs before calculating correlation
  cor_data1 <- na.omit(merge(merged_data$sigma_rGold_Price, merged_data$GPR))
  if (nrow(cor_data1) > 2) { # Need at least 3 observations for correlation
    cor1 <- cor(cor_data1, use = "complete.obs")
    cat("\nCorrelation between sigma_rGold_Price and GPR:\n")
    print(cor1)
  } else {
    cat("\nNot enough overlapping observations between sigma_rGold_Price and GPR for correlation.\n")
  }
}
# Calculate correlation between Gold volatility and S&P 500 volatility
if (all(c("sigma_rGold_Price", "sigma_rSP500_Price") %in% colnames(merged_data))) {
  cor_data2 <- na.omit(merge(merged_data$sigma_rGold_Price, merged_data$sigma_rSP500_Price))
  if (nrow(cor_data2) > 2) {
    cor2 <- cor(cor_data2, use = "complete.obs")
    cat("\nCorrelation between sigma_rGold_Price and sigma_rSP500_Price:\n")
    print(cor2)
  } else {
    cat("\nNot enough overlapping observations between sigma_rGold_Price and sigma_rSP500_Price for correlation.\n")
  }
}
# Calculate correlation between Gold volatility (simple GARCH) and Gold volatility (GARCH-X with GPR)
if (all(c("sigma_rGold_Price", "sigma_GARCHX_Gold") %in% colnames(merged_data))) {
  cor_data3 <- na.omit(merge(merged_data$sigma_rGold_Price, merged_data$sigma_GARCHX_Gold))
  if (nrow(cor_data3) > 2) {
    cor3 <- cor(cor_data3, use = "complete.obs")
    cat("\nCorrelation between sigma_rGold_Price and sigma_GARCHX_Gold:\n")
    print(cor3)
  } else {
    cat("\nNot enough overlapping observations between sigma_rGold_Price and sigma_GARCHX_Gold for correlation.\n")
  }
}


# --- C. Variance Visualization ---
cat("\n--- Conditional variance plots ---\n")
# Function to plot conditional variance (sigma) series
plot_sigma <- function(series, label, filename = NULL) {
  # Ensure series is not empty and has data
  if (is.null(series) || nrow(series) == 0) {
    cat(paste("Skipping plot for", label, "- No data.\n"))
    return()
  }
  # Create data frame for ggplot
  df <- data.frame(Date = index(series), Sigma = as.numeric(series))
  # Generate plot using ggplot2
  p <- ggplot(df, aes(x = Date, y = Sigma)) +
    geom_line(color = "darkblue", size = 0.8) +
    ggtitle(paste("Conditional variance:", label)) + # Plot title
    labs(x = "Date", y = "Variance (sigma)") + # Axis labels
    theme_light(base_size = 12) + # Theme
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90")
    )
  
  # Save plot to file if filename is provided, otherwise print
  if (!is.null(filename)) {
    ggsave(filename, plot = p, width = 10, height = 5)
    cat(paste("Variance plot saved to file:", filename, "\n"))
  } else {
    print(p)
  }
}

# Generate plots for different sigma series if they exist
if ("sigma_rGold_Price" %in% colnames(merged_data)) plot_sigma(merged_data$sigma_rGold_Price, "Gold", "sigma_gold.png")
if ("sigma_rSP500_Price" %in% colnames(merged_data)) plot_sigma(merged_data$sigma_rSP500_Price, "S&P 500", "sigma_sp500.png")
if ("sigma_GARCHX_Gold" %in% colnames(merged_data)) plot_sigma(merged_data$sigma_GARCHX_Gold, "Gold (GARCH-X with GPR)", "sigma_gold_garchx.png")


# --- 14. GARCH-X with macro regressors (GPR, VIX, FedRate, DXY, 2Y Bonds) ---
# Define potential macro regressors
regressors <- c("GPR", "VIXCLS", "DFF", "DTWEXBGS", "DGS2")
# Find which of these are actually available in the merged data
valid_regs <- regressors[regressors %in% colnames(merged_data)]

# Check if gold returns and at least one valid regressor exist
if ("rGold_Price" %in% colnames(merged_data) && length(valid_regs) > 0) {
  # Merge gold returns and valid regressors, removing rows with NAs
  garchx_data <- na.omit(merged_data[, c("rGold_Price", valid_regs)])
  # Create matrix of external regressors
  ext_matrix <- as.matrix(garchx_data[, valid_regs])
  
  # Ensure the number of rows match after NA removal
  if (nrow(ext_matrix) == nrow(garchx_data)) {
    # Define GARCH-X specification with macro regressors in the mean equation
    spec_xreg <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), # sGARCH(1,1) variance
      mean.model     = list(armaOrder = c(0, 0), external.regressors = ext_matrix), # Macro vars in mean
      distribution.model = "std" # Student's t-distribution
    )
    
    # Fit the GARCH-X model
    fit_xreg <- ugarchfit(spec = spec_xreg, data = garchx_data$rGold_Price)
    print(fit_xreg) # Print fit summary
    # Extract conditional volatility (sigma) and align with dates
    sigma_xts <- xts(sigma(fit_xreg), order.by = index(garchx_data))
    # Merge the new sigma series back into the main dataset
    merged_data <- merge(merged_data, sigma_xts, join = "outer")
    # Rename the newly added column
    colnames(merged_data)[ncol(merged_data)] <- "sigma_GARCHX_macro"
    
    cat("Volatility from GARCH-X (GPR, VIX, FedRate, DXY, 2Y Bonds) saved as sigma_GARCHX_macro\n")
  } else {
    # Stop execution if row counts don't match (should not happen with na.omit)
    stop("Error: number of observations between data and regressors does not match.")
  }
}
# Calculate correlations between the different estimated volatility series
cor_vars_to_check <- c("sigma_rGold_Price", "sigma_GARCHX_Gold", "sigma_GARCHX_macro")
existing_cor_vars <- cor_vars_to_check[cor_vars_to_check %in% colnames(merged_data)]
if (length(existing_cor_vars) >= 2) {
  print(cor(merged_data[, existing_cor_vars], use = "complete.obs"))
}


# --- 15. COMPARATIVE VISUALIZATION of GARCH models ---
# Select the different sigma series to compare
viz_vars <- c("sigma_rGold_Price", "sigma_GARCHX_Gold", "sigma_GARCHX_macro")
existing_viz_vars <- viz_vars[viz_vars %in% colnames(merged_data)]

if(length(existing_viz_vars) >= 2) { # Only plot if at least two exist
  viz_data <- na.omit(merged_data[, existing_viz_vars])
  # Rename columns for easier plotting labels if they exist
  if("sigma_rGold_Price" %in% colnames(viz_data)) colnames(viz_data)[colnames(viz_data)=="sigma_rGold_Price"] <- "GARCH_Simple"
  if("sigma_GARCHX_Gold" %in% colnames(viz_data)) colnames(viz_data)[colnames(viz_data)=="sigma_GARCHX_Gold"] <- "GARCHX_GPR"
  if("sigma_GARCHX_macro" %in% colnames(viz_data)) colnames(viz_data)[colnames(viz_data)=="sigma_GARCHX_macro"] <- "GARCHX_Macro"
  
  # Convert xts object to a data frame suitable for ggplot
  viz_df <- fortify.zoo(viz_data) # fortify works for xts too
  
  # Create individual plots for each model's volatility
  plot_list <- list()
  if("GARCH_Simple" %in% colnames(viz_df)) {
    plot_list$p1 <- ggplot(viz_df, aes(Index, GARCH_Simple)) + geom_line(color = "blue") + ggtitle("Simple GARCH Volatility") + theme_minimal() + ylab("Sigma")
  }
  if("GARCHX_GPR" %in% colnames(viz_df)) {
    plot_list$p2 <- ggplot(viz_df, aes(Index, GARCHX_GPR)) + geom_line(color = "forestgreen") + ggtitle("GARCH-X (GPR) Volatility") + theme_minimal() + ylab("Sigma")
  }
  if("GARCHX_Macro" %in% colnames(viz_df)) {
    plot_list$p3 <- ggplot(viz_df, aes(Index, GARCHX_Macro)) + geom_line(color = "red") + ggtitle("GARCH-X (Macro) Volatility") + theme_minimal() + ylab("Sigma")
  }
  
  # Combine plots using patchwork if more than one plot generated
  if(length(plot_list) > 0) {
    combined_plot <- wrap_plots(plot_list, ncol = 1) + plot_annotation(title = "Comparison of GARCH models for gold volatility")
    # Save plot to PNG file
    ggsave("comparison_garch_models.png", combined_plot, width = 12, height = 4 * length(plot_list)) # Adjust height based on number of plots
    cat("\nComparison plot saved to comparison_garch_models.png\n")
  }
  
  
  # Print correlations between the different volatility estimates
  cat("\n--- Model Volatility Correlations ---\n")
  print(cor(viz_data))
  
  # Plot ACF/PACF for the estimated volatility series
  png("acf_pacf_garch_volatility.png", width = 1000, height = 200 * ncol(viz_data)) # Adjust height
  par(mfrow = c(ncol(viz_data), 2)) # Layout based on number of series
  for(col_name in colnames(viz_data)) {
    acf(viz_data[,col_name], main=paste("ACF:", col_name))
    pacf(viz_data[,col_name], main=paste("PACF:", col_name))
  }
  dev.off()
  cat("ACF/PACF for volatility series saved to file acf_pacf_garch_volatility.png\n")
} else {
  cat("\nNot enough volatility series generated for comparison.\n")
}

# --- 16. PREP VAR DATASET ---
# Define variables for the Vector Autoregression model (using macro-GARCH volatility)
var_vars <- c("sigma_GARCHX_macro", "GPR", "VIXCLS", "DFF", "DTWEXBGS", "DGS2")
# Check which variables are available
available_var_vars <- var_vars[var_vars %in% colnames(merged_data)]
# Create VAR dataset by selecting available columns and removing rows with NAs
if(length(available_var_vars) >= 2) { # Need at least 2 variables for VAR
  var_data <- na.omit(merged_data[, available_var_vars])
  cat("\nVAR dataset prepared with variables:", paste(available_var_vars, collapse=", "), "\n")
} else {
  var_data <- NULL # Set to NULL if not enough variables
  cat("\nNot enough variables available for VAR model.\n")
}


# --- 17. ADF STATIONARITY TEST ---
# Perform ADF test on the variables prepared for VAR
if(!is.null(var_data)) {
  cat("\n--- ADF tests for VAR data ---\n")
  for (v in colnames(var_data)) { # Iterate through columns in the actual var_data
    adf <- adf.test(var_data[, v])
    cat("\n[", v, "]\n", sep="")
    print(adf)
  }
}

# --- 18. LAG SELECTION ---
# Determine the optimal number of lags for the VAR model
if(!is.null(var_data)) {
  cat("\n--- VAR lag selection ---\n")
  print(VARselect(var_data, lag.max = 10, type = "const")) # Show lag selection criteria results
}

# --- 19. VAR MODEL ESTIMATION ---
# Estimate the VAR model using a selected lag order (e.g., p=1 based on HQ/SC criteria from VARselect)
if(!is.null(var_data)) {
  cat("\n--- VAR model estimation ---\n")
  var_lag_order <- 1 # Set lag order (adjust based on VARselect results)
  var_model <- VAR(var_data, p = var_lag_order, type = "const") # Estimate VAR
  print(summary(var_model)) # Print model summary
} else {
  var_model <- NULL
  cat("\nVAR model not estimated due to lack of data.\n")
}


# --- 20. IMPULSE RESPONSE FUNCTIONS (IRF) ---
# Calculate and plot IRFs from the standard VAR model
if(!is.null(var_model) && "GPR" %in% colnames(var_data) && "sigma_GARCHX_macro" %in% colnames(var_data)) {
  cat("\n--- Impulse Response Functions (IRF) ---\n")
  # Calculate IRF of volatility to a shock in GPR
  irf_result <- irf(var_model, impulse = "GPR", response = "sigma_GARCHX_macro", n.ahead = 20, boot = TRUE)
  png("irf_gpr_to_vol.png", width = 1200, height = 800)
  plot(irf_result)
  dev.off()
  cat("\nStandard IRF plot saved to irf_gpr_to_vol.png\n")
}

# --- SVAR IDENTIFICATION ---
# Define an economically plausible ordering for Cholesky decomposition based SVAR
# This ordering imposes restrictions on contemporaneous effects.
# 1. DGS2 (2Y Bonds): Assumed to react slowly to others within the same period.
# 2. DFF (FedRate): Monetary policy, potentially influenced by bonds but influencing faster variables.
# 3. DTWEXBGS (DXY): Dollar index, influenced by rates/bonds, influences market sentiment/commodities.
# 4. VIXCLS: Fear index, reacts quickly to financial news/DXY/rates.
# 5. GPR: Geopolitical index, potentially influenced by market fear/economy, influences volatility.
# 6. sigma_GARCHX_macro: Gold volatility, assumed most reactive contemporaneously.

# Check if all necessary variables for the chosen order exist in var_data
svar_order <- c("DGS2", "DFF", "DTWEXBGS", "VIXCLS", "GPR", "sigma_GARCHX_macro")
if (!is.null(var_model) && all(svar_order %in% colnames(var_data))) {
  # --- 21. SVAR MODEL ESTIMATION ---
  cat("\n--- SVAR model estimation (Cholesky) ---\n")
  # Estimate SVAR using Cholesky decomposition based on the defined order
  # Note: `id.chol` uses the variable order directly from the VAR model input.
  # If `var_data` columns are not in the desired `svar_order`, reorder `var_data` before VAR estimation.
  # Assuming `var_data` columns match `svar_order` for simplicity here. Check `colnames(var_data)` vs `svar_order`.
  # If they don't match, re-estimate VAR: var_model <- VAR(var_data[, svar_order], p = var_lag_order, type = "const")
  svar_model <- id.chol(var_model) # Simpler call if VAR was estimated with correctly ordered data
  # If VAR was estimated with different order, need to reorder manually or use id.manual methods.
  # For now, assuming `var_model` columns match `svar_order`.
  print(summary(svar_model))
  
  # --- 22. IRF from SVAR ---
  cat("\n--- IRF from SVAR: impact of GPR shock on volatility ---\n")
  # Calculate IRF from the identified SVAR model
  svar_irf <- irf(svar_model, impulse = "GPR", response = "sigma_GARCHX_macro", n.ahead = 20, boot = TRUE)
  
  # Save the SVAR IRF plot
  png("svar_irf_gpr_to_vol.png", width = 1200, height = 800)
  plot(svar_irf)
  dev.off()
  cat("IRF from SVAR saved to file svar_irf_gpr_to_vol.png\n")
  
  # --- 23. FEVD (Forecast Error Variance Decomposition) ---
  cat("\n--- FEVD for gold volatility (from SVAR) ---\n")
  # Calculate FEVD from the SVAR model
  svar_fevd <- fevd(svar_model, n.ahead = 20)
  # Plot FEVD results (standard plot method)
  png("fevd_svar_volatility.png", width=800, height=600)
  plot(svar_fevd) # This generates plots for all variables; focus on the one for sigma_GARCHX_macro
  dev.off()
  cat("\nFEVD plot saved to fevd_svar_volatility.png\n")
  
  # Optionally, plot only the FEVD for volatility using ggplot if needed
  # Requires extracting data from svar_fevd object
  
} else {
  cat("\nSVAR model requires VAR model and specific variable order. Skipping SVAR steps.\n")
  svar_model <- NULL # Ensure svar_model is NULL if not estimated
}


# --- 24. Event Study – defining events ---
# Example: geopolitical events (e.g., war start, attack, political summit)
event_dates <- as.Date(c("2023-02-24", "2023-10-07", "2024-03-15")) # Example event dates
window <- 10  # Event window width: [-10, +10] days around the event

# --- 25. Determine AR and CAR ---
cat("\n--- Event Study: Calculating AR and CAR for Volatility ---\n")
event_study_df <- data.frame() # Initialize empty data frame
# Check if volatility data exists
if ("sigma_GARCHX_macro" %in% colnames(merged_data)) {
  vol_series <- merged_data$sigma_GARCHX_macro # Use the macro-GARCH volatility
  # Calculate expected volatility (simple mean for this example)
  expected_vol <- mean(vol_series, na.rm = TRUE)
  
  # Loop through each defined event date
  for (date in event_dates) {
    center_idx <- which(index(vol_series) == date) # Find index of event date
    # Check if event date exists and if the window fits within the data range
    if (length(center_idx) == 1 && center_idx > window && center_idx + window <= nrow(vol_series)) {
      # Define the event window range
      event_window_indices <- (center_idx - window):(center_idx + window)
      # Extract volatility data for the window
      event_window_vol <- vol_series[event_window_indices, ]
      window_dates <- index(event_window_vol) # Get dates for the window
      # Calculate Abnormal Returns (AR) = Actual Volatility - Expected Volatility
      ar <- as.numeric(event_window_vol) - expected_vol
      # Calculate Cumulative Abnormal Returns (CAR)
      car <- cumsum(ar)
      # Add results to the data frame
      event_study_df <- rbind(event_study_df,
                              data.frame(date = window_dates, AR = ar, CAR = car, event = as.character(date)))
    } else {
      cat("\nSkipping event date", as.character(date), "- date not found or window out of bounds.\n")
    }
  }
} else {
  cat("\nVolatility series 'sigma_GARCHX_macro' not found. Skipping Event Study calculation.\n")
}


# --- 26. Event Study Visualization ---
if (nrow(event_study_df) > 0) { # Check if data frame has results
  cat("\n--- Event Study Visualization ---\n")
  # Plot AR (Abnormal Returns)
  p_ar <- ggplot(event_study_df, aes(x = date, y = AR, color = event)) +
    geom_line(linewidth = 1.1) + # Line plot for AR
    geom_vline(xintercept = as.numeric(as.Date(unique(event_study_df$event))), # Vertical lines at event dates
               linetype = "dashed", color = "black") +
    theme_light(base_size = 14) +
    labs(title = "Abnormal Volatility (AR) around Geopolitical Events", # Plot title
         x = "Date", y = "AR (Volatility - Mean Volatility)") + # Axis labels
    theme(legend.position = "bottom") # Legend position
  
  ggsave("event_study_ar_volatility.png", plot = p_ar, width = 10, height = 5) # Save AR plot
  
  # Plot CAR (Cumulative Abnormal Returns)
  p_car <- ggplot(event_study_df, aes(x = date, y = CAR, color = event)) +
    geom_line(linewidth = 1.1) + # Line plot for CAR
    geom_vline(xintercept = as.numeric(as.Date(unique(event_study_df$event))), # Vertical lines at event dates
               linetype = "dashed", color = "black") +
    theme_light(base_size = 14) +
    labs(title = "Cumulative Abnormal Volatility (CAR) around Geopolitical Events", # Plot title
         x = "Date", y = "CAR") + # Axis labels
    theme(legend.position = "bottom") # Legend position
  
  ggsave("event_study_car_volatility.png", plot = p_car, width = 10, height = 5) # Save CAR plot
  cat("\nEvent study plots saved.\n")
} else {
  cat("\nNo data for event study visualization.\n")
}

# --- 27. t-Test for AR around events ---
if (nrow(event_study_df) > 0) { # Check if data frame has results
  cat("\n--- t-Test for AR (is mean AR != 0 in event window?) ---\n")
  # Group by event and perform t-test on AR for each event window
  event_tests <- event_study_df %>%
    group_by(event) %>%
    summarise(
      mean_AR = mean(AR, na.rm = TRUE), # Calculate mean AR for the window
      # Perform t-test (H0: mean AR = 0) and extract statistic and p-value
      t_stat = tryCatch(t.test(AR)$statistic, error=function(e) NA),
      p_value = tryCatch(t.test(AR)$p.value, error=function(e) NA)
    )
  print(event_tests) # Print test results
} else {
  cat("\nNo data for event study t-tests.\n")
}

# --- Prediction quality assessment ---
# Check if the necessary sigma series exist for comparison
actual_vol_var <- "sigma_rGold_Price" # Assuming simple GARCH as baseline 'actual' (or use realized vol if available)
garch_pred_var <- "sigma_GARCHX_Gold"
garchx_pred_var <- "sigma_GARCHX_macro"

if(all(c(actual_vol_var, garch_pred_var, garchx_pred_var) %in% colnames(merged_data))) {
  actual <- merged_data[, actual_vol_var]
  garch <- merged_data[, garch_pred_var]
  garchx <- merged_data[, garchx_pred_var]
  
  # Function to calculate RMSE, MAE, MSE
  metrics <- function(pred, true) {
    # Align predictions and true values, remove NAs
    comp_data <- na.omit(merge(pred, true))
    if(nrow(comp_data) < 2) return(c(RMSE=NA, MAE=NA, MSE=NA))
    pred_aligned <- comp_data[,1]
    true_aligned <- comp_data[,2]
    c(
      RMSE = sqrt(mean((pred_aligned - true_aligned)^2, na.rm = TRUE)),
      MAE = mean(abs(pred_aligned - true_aligned), na.rm = TRUE),
      MSE = mean((pred_aligned - true_aligned)^2, na.rm = TRUE)
    )
  }
  
  # Calculate metrics for GARCH-X(GPR) vs baseline
  garch_gpr_metrics <- metrics(garch, actual)
  # Calculate metrics for GARCH-X(Macro) vs baseline
  garchx_macro_metrics <- metrics(garchx, actual)
  
  # Combine metrics into a table
  comparison <- rbind(GARCH_X_GPR = garch_gpr_metrics, GARCH_X_Macro = garchx_macro_metrics)
  cat("\n--- Prediction Quality Metrics (vs Simple GARCH Volatility) ---\n")
  print(round(comparison, 6))
  
} else {
  cat("\nSkipping prediction quality assessment - required volatility series not found.\n")
}


# --- Additional IRFs from SVAR (if SVAR model exists) ---
if (!is.null(svar_model)) {
  # --- IRF: VIX → volatility ---
  if("VIXCLS" %in% colnames(var_data)) {
    cat("\n--- IRF: VIXCLS -> sigma_GARCHX_macro ---\n")
    svar_irf_vix <- irf(svar_model, impulse = "VIXCLS", response = "sigma_GARCHX_macro", n.ahead = 20, boot = TRUE)
    png("svar_irf_vix_to_vol.png", width = 1200, height = 800)
    plot(svar_irf_vix)
    dev.off()
    cat("\nSVAR IRF plot (VIX to Vol) saved.\n")
  }
  
  # --- IRF: DFF → volatility ---
  if("DFF" %in% colnames(var_data)) {
    cat("\n--- IRF: DFF -> sigma_GARCHX_macro ---\n")
    svar_irf_dff <- irf(svar_model, impulse = "DFF", response = "sigma_GARCHX_macro", n.ahead = 20, boot = TRUE)
    png("svar_irf_dff_to_vol.png", width = 1200, height = 800)
    plot(svar_irf_dff)
    dev.off()
    cat("\nSVAR IRF plot (DFF to Vol) saved.\n")
  }
}