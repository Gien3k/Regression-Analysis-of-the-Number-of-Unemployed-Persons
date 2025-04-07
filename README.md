# Regression Analysis of the Number of Unemployed Persons in Poland (2010-2022)

[![Language](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

## 1. Overview

This project conducts a regression analysis to identify socio-economic factors influencing the number of unemployed persons in Poland, utilizing data spanning the years 2010-2022. The primary goal is to develop and evaluate a linear regression model capable of explaining unemployment levels and forecasting future values. The analysis includes data loading, extensive preprocessing, feature selection, model building, comprehensive diagnostics, and forecasting for the year 2023.

The entire analysis workflow, code, and detailed explanations are available in the R Markdown file (`Project.Rmd`). A plain R script (`analysis_script.R`) containing only the executable code is also provided. A pre-rendered HTML report (`Project.html`) is included for convenience (see notes in the File Structure and Results sections regarding GitHub language statistics).

## 2. Key Stages & Features

* **Data Ingestion:** Loads time-series data from multiple Excel files covering various socio-economic indicators.
* **Data Preprocessing:** Cleans data, handles missing values (including specific string replacements like "-"), converts data types, and merges disparate datasets based on the 'Year'.
* **Feature Selection:** Optimizes the predictor set by removing variables with near-zero variance and high inter-correlation (using the `caret` package).
* **Model Building:** Constructs a multiple linear regression model (`lm` function in R) to predict the number of unemployed persons based on the selected features.
* **Model Diagnostics:** Performs rigorous validation of the regression model's assumptions using statistical tests (Shapiro-Wilk, Jarque-Bera for normality; RESET for specification; VIF for multicollinearity; Breusch-Pagan for heteroscedasticity; Durbin-Watson for autocorrelation) and standard diagnostic plots.
* **Forecasting:** Extrapolates predictor variables to the target year (2023) using simple linear time trends and generates a forecast for the number of unemployed persons using the final regression model.
* **Evaluation:** Compares the forecasted unemployment value for 2023 against the actual recorded value, calculating the forecast error.
* **Visualization:** Employs `ggplot2` and `ggcorrplot` to visualize data distributions, time trends, correlations between variables, actual vs. predicted values, and the final forecast.

## 3. Data

The analysis utilizes annual data for Poland spanning the years 2010 to 2022. The data were primarily sourced from **Statistics Poland (Główny Urząd Statystyczny - GUS)** public resources, available at [stat.gov.pl](https://stat.gov.pl). For this analysis, the relevant data points were compiled into local Excel (`.xlsx`) files.

Key variables explored include (but are not limited to):

* Number of Unemployed Persons (Dependent Variable)
* Average Salaries
* Number of Divorces
* Number of Crimes Committed
* Alcohol Dependence Statistics
* Number of Foster Families
* Vodka Prices
* Psychiatric Treatment Numbers
* AIDS Cases
* Housing Prices
* Number of Prisoners
* Liquidated Jobs
* Employment Costs

**Note:** The compiled Excel files (e.g., `unemployed.xlsx`, `vodka_price.xlsx`, `avg_salary.xlsx`, etc., using English filenames as expected by the code), originally sourced from GUS, **are included** in the `data/` folder within this repository. Ensure paths within the R code (`Project.Rmd` or `analysis_script.R`) correctly point to this subfolder (e.g., using `file.path("data", "filename.xlsx")`).

## 4. Methodology

The core of the analysis is **multiple linear regression**. Predictor variables undergo a selection process involving the removal of near-zero variance features and features exhibiting high pairwise correlation (cutoff > 0.70). The final model's validity is assessed through a suite of standard diagnostic tests and graphical analysis to check for violations of OLS assumptions. Forecasting for the subsequent year is performed via linear extrapolation of predictors.

## 5. Technologies Used

* **Language:** R
* **Reporting:** R Markdown (`knitr`)
* **Key R Packages:**
    * Data Handling: `readxl`, `dplyr`, `purrr`, `tidyr`, `writexl`
    * Visualization: `ggplot2`, `ggcorrplot`, `scales`
    * Modeling & Diagnostics: `stats` (base R `lm`, `anova`), `caret`, `car`, `lmtest`, `tseries`, `strucchange`
    * Forecasting (basic): `forecast` (used lightly)

## 6. File Structure

* `Project.Rmd`: The main R Markdown file containing the complete analysis, including R code chunks, methodology explanations, and interpretation of results. **This is the primary file to read and run for the full context.**
* `analysis_script.R`: A plain R script derived from `Project.Rmd`, containing only the executable R code. Useful for running the analysis steps without the narrative text.
* `Project.html`: The rendered HTML report generated by knitting `Project.Rmd`. *(Note: Including large output files directly in the repository can skew GitHub language statistics. See 'Results' section and 'How to Run' for generating it yourself).*
* `data/`: Folder containing the required Excel data files (`.xlsx`), originally sourced from Statistics Poland (GUS) and renamed to English convention.
* `LICENSE`: Project license information (MIT).
* `README.md`: This file, providing an overview of the project.
* *(Optional but Recommended: `.gitignore`): File specifying intentionally untracked files (like generated reports `*.html`, `*.pdf`, and temporary R files like `.Rhistory`, `.RData`).*

## 7. How to Run

1.  **Clone Repository:** Clone this repository to your local machine.
    ```bash
    git clone [https://github.com/Gien3k/Regression-Analysis-of-the-Number-of-Unemployed-Persons.git](https://github.com/Gien3k/Regression-Analysis-of-the-Number-of-Unemployed-Persons.git)
    cd Regression-Analysis-of-the-Number-of-Unemployed-Persons
    ```
2.  **Prerequisites:** Ensure you have R and RStudio (recommended) installed.
3.  **Data Files:** The required `.xlsx` data files are included in the `data/` folder. The R code (`Project.Rmd` and `analysis_script.R`) is already configured to look for files in this subfolder.
4.  **Install Packages:** Open R or RStudio and install the necessary packages by running the following command in the console:
    ```R
    install.packages(c("readxl", "ggplot2", "car", "lmtest", "dplyr", "purrr", "caret", "tseries", "strucchange", "forecast", "writexl", "scales", "ggcorrplot", "tidyr", "knitr"))
    ```
5.  **Run the Analysis (Choose one option):**

    * **Option 1 (Recommended - Full Report):**
        * Open `Project.Rmd` in RStudio.
        * Use the "Knit" button to execute the analysis and generate the full HTML (or PDF) report. This report includes code, output, plots, and explanatory text, providing the complete context.

    * **Option 2 (Script Only):**
        * Open `analysis_script.R` in RStudio or run it from the R console using `source('analysis_script.R')`. Make sure your working directory is the main project folder (`Regression-Analysis-of-the-Number-of-Unemployed-Persons`).
        * This will execute the analysis steps, print outputs and test results to the console, and display plots in the R graphics device (or save them if the script is modified to do so).
        * Note: This script lacks the narrative context and formatted presentation provided by the R Markdown file.

## 8. Results

The primary output is the generated report (`Project.html` or PDF) from knitting the `Project.Rmd` file. This report includes:

* Detailed summary statistics of the data.
* Results of the feature selection process.
* The final linear regression model summary (coefficients, R-squared, F-statistic).
* Outputs from all diagnostic tests performed on the model.
* Diagnostic plots for visual assessment of model assumptions.
* A plot comparing actual vs. predicted unemployment values.
* The forecasted unemployment value for 2023 and its comparison with the actual value (including error calculation).
* Visualizations of variable correlations and time trends.


## 9. License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
