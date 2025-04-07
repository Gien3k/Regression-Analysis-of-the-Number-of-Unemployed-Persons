# --- R Script Generated from R Markdown ---
# Title: Regression Analysis of the Number of Unemployed Persons
# Author: Mateusz Grabowski

# --- 2. Loading Packages ---
# Load necessary R packages, suppressing startup messages
suppressPackageStartupMessages({
  library(readxl)      # For reading Excel files
  library(ggplot2)     # For creating plots
  library(car)         # For regression diagnostics (e.g., VIF, Anova)
  library(lmtest)      # For diagnostic tests (e.g., Breusch-Pagan, Durbin-Watson)
  library(dplyr)       # For data manipulation
  library(purrr)       # For functional programming (used here for joining data)
  library(caret)       # For data pre-processing (e.g., nearZeroVar, findCorrelation)
  library(tseries)     # For time series analysis tests (e.g., Jarque-Bera)
  library(strucchange) # For testing structural changes in models (RESET test)
  library(forecast)    # For forecasting tools (though not heavily used here)
  library(writexl)     # For writing Excel files (if needed)
  library(scales)      # For scaling axes in plots (e.g., comma format)
  library(ggcorrplot)  # For visualizing correlation matrices
  library(tidyr)       # For tidying data (e.g., pivot_longer)
})

# --- 3. Loading Data ---
# (Using English filenames and 'data/' subdirectory path)

# Function to read data from Excel files, select Year and Value columns, rename Value column
# Now includes prepending the 'data/' subdirectory to the filename
# Wrap in tryCatch for better error handling if files might be missing
load_excel_data <- function(filename_relative, value_col_name) {
  # Construct the full path relative to the project root
  full_path <- file.path("data", filename_relative)
  tryCatch({
    read_excel(full_path) %>%
      # Assuming Excel columns are named 'Rok' and 'Wartosc'
      select(Year = Rok, Value = Wartosc) %>%
      rename({{value_col_name}} := Value)
  }, error = function(e) {
    warning(paste("Could not load or process file:", full_path, "\nError:", e$message))
    # Return an empty tibble with expected columns
    tibble(Year = integer(), {{value_col_name}} := numeric())
  })
}

# Load data using NEW ENGLISH file names from the 'data' subfolder
vodka_price_data <- load_excel_data("vodka_price.xlsx", "vodka_price")
psychiatric_treatment_data <- load_excel_data("psychiatric_treatment.xlsx", "psychiatric_treatment")
crimes_data <- load_excel_data("crimes.xlsx", "crimes")
divorces_data <- load_excel_data("divorces.xlsx", "divorces")
alcohol_dependence_data <- load_excel_data("alcohol_dependence.xlsx", "alcohol_dependence")
salary_data <- load_excel_data("avg_salary.xlsx", "avg_salary")
aids_data <- load_excel_data("aids_cases.xlsx", "aids_cases")
unemployed_data <- load_excel_data("unemployed.xlsx", "unemployed")
housing_price_data <- load_excel_data("housing_price.xlsx", "housing_price")
prisoners_data <- load_excel_data("prisoners.xlsx", "prisoners")
foster_families_data <- load_excel_data("foster_families.xlsx", "foster_families")
liquidated_jobs_data <- load_excel_data("liquidated_jobs.xlsx", "liquidated_jobs")
employment_costs_data <- load_excel_data("employment_costs.xlsx", "employment_costs")

# Check if 'unemployed_data' loaded correctly, stop if not
if(nrow(unemployed_data) == 0) {
  stop("Failed to load the primary dependent variable file: data/unemployed.xlsx. Please check the file path and format.")
}

# --- 4. Data Conversion ---

# Function to convert the 'Year' column to numeric, handles potential errors
convert_year_to_numeric <- function(df) {
  # Check if df is a valid data frame and has the 'Year' column
  if (!is.data.frame(df) || !"Year" %in% colnames(df)) {
    warning("Input is not a valid data frame or 'Year' column is missing.")
    return(df) # Return unchanged df or handle error differently
  }
  df %>%
    mutate(Year = suppressWarnings(as.numeric(as.character(Year)))) # Convert factor/char safely
}

# Apply conversion individually, checking if the df exists first.
if (exists("vodka_price_data") && is.data.frame(vodka_price_data)) vodka_price_data <- convert_year_to_numeric(vodka_price_data)
if (exists("psychiatric_treatment_data") && is.data.frame(psychiatric_treatment_data)) psychiatric_treatment_data <- convert_year_to_numeric(psychiatric_treatment_data)
if (exists("crimes_data") && is.data.frame(crimes_data)) crimes_data <- convert_year_to_numeric(crimes_data)
if (exists("divorces_data") && is.data.frame(divorces_data)) divorces_data <- convert_year_to_numeric(divorces_data)
if (exists("alcohol_dependence_data") && is.data.frame(alcohol_dependence_data)) alcohol_dependence_data <- convert_year_to_numeric(alcohol_dependence_data)
if (exists("salary_data") && is.data.frame(salary_data)) salary_data <- convert_year_to_numeric(salary_data)
if (exists("aids_data") && is.data.frame(aids_data)) aids_data <- convert_year_to_numeric(aids_data)
if (exists("unemployed_data") && is.data.frame(unemployed_data)) unemployed_data <- convert_year_to_numeric(unemployed_data)
if (exists("housing_price_data") && is.data.frame(housing_price_data)) housing_price_data <- convert_year_to_numeric(housing_price_data)
if (exists("prisoners_data") && is.data.frame(prisoners_data)) prisoners_data <- convert_year_to_numeric(prisoners_data)
if (exists("foster_families_data") && is.data.frame(foster_families_data)) foster_families_data <- convert_year_to_numeric(foster_families_data)
if (exists("liquidated_jobs_data") && is.data.frame(liquidated_jobs_data)) liquidated_jobs_data <- convert_year_to_numeric(liquidated_jobs_data)
if (exists("employment_costs_data") && is.data.frame(employment_costs_data)) employment_costs_data <- convert_year_to_numeric(employment_costs_data)

# --- 5. Merging Data ---

# Create a list of data frames that were successfully loaded and converted
# Filter out any NULLs or non-dataframes resulting from load errors
valid_df_list <- Filter(function(x) is.data.frame(x) && nrow(x) > 0,
                        list(unemployed_data, vodka_price_data, psychiatric_treatment_data,
                             crimes_data, divorces_data, alcohol_dependence_data, salary_data,
                             aids_data, housing_price_data, prisoners_data, foster_families_data,
                             liquidated_jobs_data, employment_costs_data))

# Check if the list is not empty and contains 'unemployed_data'
if (length(valid_df_list) > 0 && any(sapply(valid_df_list, function(df) "unemployed" %in% colnames(df)))) {
  # Use reduce to sequentially join all valid data frames by 'Year'
  df_merged <- reduce(valid_df_list, full_join, by = "Year")
  
  # Optional: Display structure/head (commented out)
  # print(str(df_merged))
  # print(head(df_merged))
  
} else {
  stop("No valid data frames available for merging, or 'unemployed_data' is missing.")
}

# --- 6. Data Cleaning and Preparation ---

# Use df_merged from now on
# Replace any potential "-" strings with NA across the entire dataframe
df_merged <- as.data.frame(lapply(df_merged, function(col) {
  if (is.character(col) || is.factor(col)) {
    na_if(col, "-")
  } else {
    col
  }
}))

# Convert all columns (except Year, which should be numeric) to numeric, coercing errors to NA
df_merged <- df_merged %>%
  mutate(across(-Year, ~suppressWarnings(as.numeric(as.character(.)))))

# Remove rows with any NA values (listwise deletion)
df_complete <- na.omit(df_merged)

# Check dimensions before and after NA removal (commented out)
# print(paste("Dimensions before NA removal:", paste(dim(df_merged), collapse="x")))
# print(paste("Dimensions after NA removal:", paste(dim(df_complete), collapse="x")))

# Stop if no complete cases remain
if(nrow(df_complete) == 0) {
  stop("No complete observations remain after removing NAs. Check input data quality.")
} else if (nrow(df_complete) < 10) { # Check if enough data points remain
  warning(paste("Only", nrow(df_complete), "complete observations remain. Model results may be unreliable."))
}

# Create df_model by removing the 'Year' column from the complete cases dataframe
if ("Year" %in% colnames(df_complete)) {
  df_model <- df_complete %>% select(-Year)
} else {
  df_model <- df_complete # Assume Year was already absent
}

# Ensure 'unemployed' column exists
if (!"unemployed" %in% colnames(df_model)) {
  stop("'unemployed' column not found in the prepared data frame 'df_model'.")
}

# Ensure all columns are numeric (redundant if done before na.omit, but safe)
df_model <- df_model %>% mutate(across(everything(), as.numeric))

# --- 7. Displaying Data Before Variable Removal ---

print("Head of data frame before variable removal:")
head(df_model)
print("Summary of data frame before variable removal:")
summary(df_model)

# --- 8. Data Optimization (Feature Selection) ---

# Ensure there are enough columns to perform selection
if (ncol(df_model) <= 2) {
  warning("Not enough predictor columns to perform feature selection.")
} else {
  
  # 1. Remove Near-Zero Variance Predictors
  predictors_initial <- setdiff(colnames(df_model), "unemployed")
  nzv_indices <- nearZeroVar(df_model[, predictors_initial], saveMetrics = FALSE)
  
  if (length(nzv_indices) > 0) {
    nzv_cols_to_remove <- predictors_initial[nzv_indices]
    print(paste("Removing Near-Zero Variance columns:", paste(nzv_cols_to_remove, collapse=", ")))
    df_model <- df_model[, !(colnames(df_model) %in% nzv_cols_to_remove)]
  } else {
    print("No Near-Zero Variance columns found to remove.")
  }
  
  # Update predictor list after NZV removal
  predictors_after_nzv <- setdiff(colnames(df_model), "unemployed")
  
  # 2. Remove Highly Correlated Predictors
  if (length(predictors_after_nzv) > 1) {
    cor_matrix <- cor(df_model[, predictors_after_nzv], use = "complete.obs")
    highly_correlated_indices <- findCorrelation(cor_matrix, cutoff = 0.70, names = FALSE)
    
    if (length(highly_correlated_indices) > 0) {
      highly_correlated_cols_to_remove <- predictors_after_nzv[highly_correlated_indices]
      print(paste("Removing Highly Correlated columns:", paste(highly_correlated_cols_to_remove, collapse=", ")))
      df_model <- df_model[, !(colnames(df_model) %in% highly_correlated_cols_to_remove)]
    } else {
      print("No highly correlated columns (cutoff=0.70) found to remove.")
    }
  } else {
    print("Not enough predictors remaining to check for high correlation.")
  }
}

# Display dimensions after feature selection
print(paste("Dimensions after variable removal:", paste(dim(df_model), collapse="x")))

# Ensure 'unemployed' is still present
if (!"unemployed" %in% colnames(df_model)) {
  stop("'unemployed' column was removed during feature selection. Check the process.")
}
# Ensure at least one predictor remains
if (ncol(df_model) < 2) {
  stop("No predictor variables remain after feature selection.")
}

# Displaying Data After Variable Removal
print("Head of data frame after variable removal:")
head(df_model)
print("Remaining columns:")
colnames(df_model)

# --- 9. Building and Analyzing the Linear Regression Model ---

# Build the model
final_model <- lm(unemployed ~ ., data = df_model)

# Model Summary
print("--- Model Summary ---")
summary(final_model)

# Shapiro-Wilk Test for Normality of Residuals
print("--- Shapiro-Wilk Test ---")
shapiro_test_result <- shapiro.test(residuals(final_model))
print(shapiro_test_result)
if (shapiro_test_result$p.value > 0.05) {
  print("Shapiro-Wilk Test: p > 0.05, Do not reject H0. Residuals appear normally distributed.")
} else {
  print("Shapiro-Wilk Test: p <= 0.05, Reject H0. Residuals do not appear normally distributed.")
}

# RESET Test for Model Specification
print("--- RESET Test ---")
reset_test_result <- resettest(final_model)
print(reset_test_result)
if (reset_test_result$p.value > 0.05) {
  print("RESET Test: p > 0.05, Do not reject H0. No evidence of misspecification found.")
} else {
  print("RESET Test: p <= 0.05, Reject H0. Evidence of potential misspecification (e.g., missing non-linearity).")
}

# VIF for Multicollinearity
print("--- Variance Inflation Factor (VIF) ---")
if (length(coef(final_model)) > 2) {
  vif_values <- vif(final_model)
  print(vif_values)
  if (any(vif_values > 10)) {
    print("VIF Interpretation: At least one VIF > 10, indicating potentially problematic multicollinearity.")
  } else if (any(vif_values > 5)) {
    print("VIF Interpretation: At least one VIF > 5, indicating moderate multicollinearity. Investigate further.")
  } else {
    print("VIF Interpretation: All VIF values are low (< 5), suggesting multicollinearity is not a major issue.")
  }
} else {
  print("VIF calculation requires at least two predictor variables in the model.")
}

# Breusch-Pagan Test for Heteroscedasticity
print("--- Breusch-Pagan Test ---")
bp_test_result <- bptest(final_model)
print(bp_test_result)
if (bp_test_result$p.value > 0.05) {
  print("Breusch-Pagan Test: p > 0.05, Do not reject H0. No significant evidence of heteroscedasticity found.")
} else {
  print("Breusch-Pagan Test: p <= 0.05, Reject H0. Evidence of heteroscedasticity (non-constant variance).")
}

# Jarque-Bera Test for Normality of Residuals
print("--- Jarque-Bera Test ---")
jb_test_result <- jarque.bera.test(residuals(final_model))
print(jb_test_result)
if (jb_test_result$p.value > 0.05) {
  print("Jarque-Bera Test: p > 0.05, Do not reject H0. Residuals appear normally distributed.")
} else {
  print("Jarque-Bera Test: p <= 0.05, Reject H0. Residuals do not appear normally distributed.")
}

# Analysis of Variance (ANOVA)
print("--- ANOVA Table ---")
anova_result <- anova(final_model) # Using base anova (Type I SS)
print(anova_result)

# Durbin-Watson Test for Autocorrelation
print("--- Durbin-Watson Test ---")
dw_test_result <- dwtest(final_model)
print(dw_test_result)
if (dw_test_result$p.value > 0.05) {
  print("Durbin-Watson Test: p > 0.05, Do not reject H0. No significant evidence of autocorrelation found.")
} else {
  print("Durbin-Watson Test: p <= 0.05, Reject H0. Evidence of autocorrelation.")
}

# --- 10. Diagnostic Plots ---
# These will open in the default R graphics device when run as a script.
# Consider saving them to files if running non-interactively.
# Example: pdf("diagnostic_plots.pdf", width=7, height=6); par(mfrow=c(2,2)); plot(final_model); dev.off()
print("Generating Diagnostic Plots...")
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(final_model)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1) # Reset layout

# --- 11. Actual vs. Predicted Plot ---
print("Generating Actual vs. Predicted Plot...")
df_plot <- df_model
df_plot$predicted_unemployed <- predict(final_model)

# Use print() to explicitly render ggplot in a script
# Or save using ggsave("actual_vs_predicted.png", width=6, height=5)
print(
  ggplot(df_plot, aes(x = unemployed, y = predicted_unemployed)) +
    geom_point(alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + # y=x line
    labs(title = "Actual vs. Predicted Unemployment Values",
         x = "Actual Unemployment",
         y = "Predicted Unemployment") +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()
)

# --- 12. Preparing Data for Forecasting (Year 2023) ---
print("Preparing data for forecasting...")
if (!exists("df_merged")) {
  stop("Required data frame 'df_merged' not found for forecasting prep.")
}

final_predictors <- setdiff(names(coef(final_model)), "(Intercept)")

df_for_trends <- df_merged %>%
  select(Year, all_of(final_predictors)) %>%
  mutate(Year = as.numeric(Year)) %>%
  arrange(Year)

available_years <- range(df_for_trends$Year, na.rm = TRUE)
print(paste("Years available for trend fitting:", available_years[1], "-", available_years[2]))

forecast_year <- 2023

# --- 13. Forecasting Predictor Values for 2023 ---
print("Forecasting predictor values via linear trend...")
new_data_trends <- data.frame(Year = forecast_year)
forecasted_predictors_2023 <- data.frame(Year = forecast_year)

for (var in final_predictors) {
  trend_data_var <- df_for_trends %>%
    select(Year, all_of(var)) %>%
    filter(!is.na(.data[[var]]))
  
  if (nrow(trend_data_var) >= 5) {
    model_var_trend <- lm(as.formula(paste0("`", var, "` ~ Year")), data = trend_data_var)
    predicted_value <- predict(model_var_trend, newdata = new_data_trends)
    forecasted_predictors_2023[[var]] <- predicted_value
  } else {
    warning(paste("Not enough data points (<5) to fit trend for variable:", var, ". Cannot forecast."))
    forecasted_predictors_2023[[var]] <- NA
  }
}

forecasted_predictors_2023 <- forecasted_predictors_2023 %>% select(-Year)

if (any(is.na(forecasted_predictors_2023))) {
  warning("Forecasting failed for some predictors. Prediction for 'unemployed' might be NA.")
}

print("Forecasted predictor values for 2023:")
print(forecasted_predictors_2023)

# --- 14. Comparing Forecast with Actual Value for 2023 ---
print("Forecasting unemployment and comparing with actual...")
if (exists("forecasted_predictors_2023") && is.data.frame(forecasted_predictors_2023) && all(!is.na(forecasted_predictors_2023))) {
  forecast_unemployed_value <- predict(final_model, newdata = forecasted_predictors_2023)
  print(paste("Forecasted number of unemployed for 2023:", round(forecast_unemployed_value)))
} else {
  forecast_unemployed_value <- NA
  print("Cannot forecast 'unemployed' due to missing or invalid forecasted predictor values.")
}

actual_unemployed_df_2023 <- NULL
if (exists("unemployed_data") && is.data.frame(unemployed_data)) {
  actual_unemployed_df_2023 <- unemployed_data %>% filter(Year == forecast_year)
}

actual_unemployed_value <- NA
if (!is.null(actual_unemployed_df_2023) && nrow(actual_unemployed_df_2023) == 1) {
  retrieved_value <- actual_unemployed_df_2023$unemployed
  converted_value <- suppressWarnings(as.numeric(as.character(retrieved_value)))
  if (!is.na(converted_value)) {
    actual_unemployed_value <- converted_value
    print(paste("Actual number of unemployed in 2023:", round(actual_unemployed_value)))
  } else {
    print(paste("Actual value for 2023 found but is non-numeric:", retrieved_value))
  }
} else if (!is.null(actual_unemployed_df_2023) && nrow(actual_unemployed_df_2023) > 1) {
  print(paste("Warning: Multiple entries found for Year =", forecast_year, "in 'unemployed_data'. Cannot determine unique actual value."))
} else {
  print(paste("Actual value for Year =", forecast_year, "not found in the 'unemployed_data'."))
}

if (!is.na(forecast_unemployed_value) && !is.na(actual_unemployed_value)) {
  forecast_error <- actual_unemployed_value - forecast_unemployed_value
  if (actual_unemployed_value != 0) {
    percent_error <- (forecast_error / actual_unemployed_value) * 100
    print(paste("Forecast Percent Error:", round(percent_error, 2), "%"))
  } else {
    percent_error <- NA
    print("Cannot calculate percent error because actual value is zero.")
  }
  print(paste("Forecast Error (Actual - Forecast):", round(forecast_error)))
} else {
  print("Cannot calculate forecast error because forecast or actual value is missing or non-numeric.")
}

# --- 15. Visualization of the Unemployment Forecast for 2023 ---
print("Generating Forecast Plot...")
df_hist_plot <- df_complete %>% arrange(Year)

if (!is.na(forecast_unemployed_value)) {
  forecast_point_df <- data.frame(Year = forecast_year, unemployed = forecast_unemployed_value, Type = "Forecast")
} else {
  forecast_point_df <- NULL
}

if (!is.na(actual_unemployed_value)) {
  actual_point_df <- data.frame(Year = forecast_year, unemployed = actual_unemployed_value, Type = "Actual")
} else {
  actual_point_df <- NULL
}

points_df <- bind_rows(forecast_point_df, actual_point_df)

# Use print() for ggplot
# Or save using ggsave("forecast_plot.png", width=7, height=5)
print(
  ggplot(df_hist_plot, aes(x = Year, y = unemployed)) +
    geom_line(color = "blue", alpha=0.8) +
    geom_point(color = "blue", alpha=0.6) +
    geom_point(data = points_df, aes(x = Year, y = unemployed, color = Type, shape = Type), size = 4) +
    scale_color_manual(values = c("Forecast" = "red", "Actual" = "darkgreen")) +
    scale_shape_manual(values = c("Forecast" = 16, "Actual" = 4)) + # Circle for forecast, X for actual
    labs(title = paste("Unemployment Trend and Forecast for", forecast_year),
         x = "Year", y = "Number of Unemployed") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(min(df_hist_plot$Year), forecast_year, by = 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

# --- 16. Correlation Matrix Between Final Variables ---
print("Generating Final Correlation Matrix Plot...")
cor_matrix_final <- cor(df_model, use = "complete.obs")

# Use print() for ggcorrplot
# Or save using appropriate method if ggcorrplot doesn't auto-print from script
# png("correlation_matrix_final.png", width=7*100, height=6*100, res=100); print(ggcorrplot(...)); dev.off()
print(
  ggcorrplot(cor_matrix_final,
             method = "circle",
             type = "upper",
             ggtheme = ggplot2::theme_minimal,
             lab = TRUE,
             lab_size = 3.5,
             title = "Correlation Matrix of Variables in Final Model") +
    theme(legend.position = "bottom")
)

# --- 17. Time Trends of Final Variables ---
print("Generating Time Trends Plot...")
df_model_trends <- df_model
if (all(rownames(df_model) %in% rownames(df_complete))) {
  df_model_trends$Year <- df_complete[rownames(df_model), "Year"]
} else {
  warning("Row names do not match between df_model and df_complete. Cannot reliably add 'Year' for trend plot.")
  df_model_trends$Year <- NA
}

if (!any(is.na(df_model_trends$Year))) {
  df_long_final <- df_model_trends %>%
    pivot_longer(cols = -Year, names_to = "Variable", values_to = "Value")
  
  # Use print() for ggplot
  # Or save using ggsave("time_trends_final.png", width=9, height=7)
  print(
    ggplot(df_long_final, aes(x = Year, y = Value, color = Variable)) +
      geom_line() +
      geom_point(alpha=0.6) +
      labs(title = "Time Trends of Variables in Final Model", x = "Year", y = "Value") +
      scale_x_continuous(breaks = seq(min(df_model_trends$Year), max(df_model_trends$Year), by = 1)) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      facet_wrap(~ Variable, scales = "free_y") +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
  )
} else {
  print("Skipping time trend plot as 'Year' could not be added back to df_model reliably.")
}

# --- 18. Histogram of the Number of Unemployed (Final Data) ---
print("Generating Final Histogram...")
# Use print() for ggplot
# Or save using ggsave("histogram_final.png", width=6, height=4)
print(
  ggplot(df_model, aes(x = unemployed)) +
    geom_histogram(aes(y = after_stat(density)), fill = "steelblue", bins = 10, alpha = 0.8, color="black") +
    geom_density(color = "red") +
    labs(title = "Distribution of Unemployed Persons (Final Data)",
         x = "Number of Unemployed Persons",
         y = "Density") +
    scale_x_continuous(labels = scales::comma) +
    theme_minimal()
)

# --- 19. Box Plot for Final Variables ---
print("Generating Final Box Plot...")
if (exists("df_long_final") && is.data.frame(df_long_final)) {
  # Use print() for ggplot
  # Or save using ggsave("boxplot_final.png", width=7, height=5)
  print(
    ggplot(df_long_final %>% filter(Variable != "Year"), aes(x = Variable, y = Value, fill = Variable)) +
      geom_boxplot(alpha=0.8) +
      coord_flip() +
      labs(title = "Box Plot for Variables in Final Model", x = "Variable", y = "Value") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(legend.position = "none")
  )
} else {
  print("Skipping box plot as data ('df_long_final') is not available.")
}

# --- End of Script ---
print("Analysis script completed.")
