# ==============================================================================
# UTILITY FUNCTIONS FOR THE NEWSEED BIOFORTIFIED MAIZE PROJECT
# ==============================================================================
#
# This file contains all utility functions used throughout the analytical
# framework for evaluating the impact of biofortified maize on child
# stunting in Guatemala.
#
# Functions are organized by category:
# 1. Statistical Testing & Model Evaluation
# 2. Data Visualization (Models & Effects)
# 3. Distribution Analysis & Comparison
# 4. Nutrient Calculations & Adjustments
# 5. Survey Data Processing
# 6. Data Management & Utilities
# 7. Scenario Simulation & Market Modeling
#
# Author: Azahar Data Insights (ADI)
# Project: New Seed - Guatemala
# ==============================================================================


# ==============================================================================
# 1. STATISTICAL TESTING & MODEL EVALUATION
# ==============================================================================

# ==============================================================================
# Apply Wald Test to Model Variables
# ==============================================================================
# Performs Wald test for individual predictor variables in survey-weighted
# regression models to assess statistical significance.
#
# Arguments:
#   variable - Character string with variable name to test
#   model - Survey-weighted regression model (svyglm object)
#
# Returns:
#   Tibble with test statistics containing:
#     - variable: name of tested variable
#     - f_statistic: F-statistic value (rounded to 2 decimals)
#     - p_value: formatted p-value
#     - significant: asterisk (*) if p < 0.05, empty otherwise
#
# Example:
#   test_result <- apply_wald_test("zinc_intake", stunting_model)
# ==============================================================================
apply_wald_test <- function(variable, model) {
  
  # Check required packages are installed
  required_packages <- c("survey", "tibble", "prettyunits", "dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  tryCatch({
    # Perform Wald test
    test <- survey::regTermTest(model, as.formula(paste("~", variable)))
    
    # Return results as tibble
    tibble::tibble(
      variable = variable,
      f_statistic = round(as.numeric(test$Ftest), 2),
      p_value = prettyunits::pretty_p_value(as.numeric(test$p)),
      significant = dplyr::if_else(as.numeric(test$p) < 0.05, "*", "")
    )
  }, error = function(e) {
    message(paste("Error applying Wald test for variable:", variable, "\n", e))
    tibble::tibble(
      variable = variable,
      f_statistic = NA,
      p_value = NA,
      significant = ""
    )
  })
}

# ==============================================================================
# Analyze Near-Zero Variance in Dataset
# ==============================================================================
# Identifies variables with near-zero variance that may cause problems in
# statistical modeling. Returns formatted table with variance metrics.
#
# Arguments:
#   df - Data frame to analyze
#
# Returns:
#   GT table with variance analysis metrics for each variable:
#     - variable: variable name
#     - freq_ratio: frequency ratio of most common to second most common value
#     - pct_unique: percentage of unique values
#     - nzv: near-zero variance flag (TRUE/FALSE)
#
# Example:
#   variance_table <- analyze_variance(my_dataset)
# ==============================================================================
analyze_variance <- function(df) {
  
  # Check required packages are installed
  required_packages <- c("caret", "tibble", "dplyr", "gt")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  df |>
    # Calculate variance metrics for all variables
    caret::nearZeroVar(saveMetrics = TRUE) |>
    tibble::rownames_to_column(var = "variable") |>
    # Remove redundant zeroVar column
    dplyr::select(-zeroVar) |>
    # Convert percentUnique to actual percentage (0-100 scale)
    dplyr::mutate(percentUnique = percentUnique * 100) |>
    # Format as GT table
    gt::gt() |>
    gt::tab_header(
      title = "Near-Zero Variance Analysis",
      subtitle = "Predictor variables with potentially insufficient variation"
    ) |>
    gt::cols_label(
      variable = "Variable",
      freqRatio = "Frequency Ratio",
      percentUnique = "% Unique Values",
      nzv = "NZV Flag"
    ) |>
    gt::fmt_number(
      columns = freqRatio,
      decimals = 3
    ) |>
    gt::fmt_number(
      columns = percentUnique,
      decimals = 2,
      pattern = "{x}%"
    ) |>
    # Highlight NZV = TRUE rows in light red
    gt::tab_style(
      style = gt::cell_fill(color = "#F8D7DA"),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = nzv == TRUE
      )
    ) |>
    # Bold variable names
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = variable)
    ) |>
    gt::tab_footnote(
      footnote = "Frequency Ratio: ratio of most common to second most common value. Values >19 combined with low uniqueness trigger NZV flag.",
      locations = gt::cells_column_labels(columns = freqRatio)
    ) |>
    gt::tab_footnote(
      footnote = "% Unique Values: percentage of unique values relative to total observations. Values <10% combined with high frequency ratio trigger NZV flag.",
      locations = gt::cells_column_labels(columns = percentUnique)
    ) |>
    gt::tab_footnote(
      footnote = "NZV Flag: TRUE indicates near-zero variance. Both freqRatio >19 AND percentUnique <10% must be met.",
      locations = gt::cells_column_labels(columns = nzv)
    ) |>
    gt::tab_options(
      table.font.size = 12,
      heading.title.font.size = 14,
      heading.subtitle.font.size = 11,
      table.width = gt::pct(100)
    )
}

# ==============================================================================
# Train Survey-Weighted GLM Model
# ==============================================================================
# Trains a survey-weighted generalized linear model, generates diagnostics,
# and creates visualization of model fit. Returns model object and results.
#
# Arguments:
#   formula - Model formula (e.g., outcome ~ predictor1 + predictor2)
#   model_label - Descriptive label for the model (used in output)
#   x_label - Label for x-axis in diagnostic plot
#   y_label - Label for y-axis in diagnostic plot
#   design - Survey design object (default: survey_design from environment)
#
# Returns:
#   List containing:
#     - model: fitted svyglm model object
#     - coefficients: GT table with model coefficients and p-values
#     - results: augmented data frame with fitted values and residuals
#     - plot: ggplot object showing observed vs predicted values
#
# Example:
#   zinc_model <- train_survey_glm(
#     zinc_total ~ age + sex + department,
#     "Zinc Intake Model",
#     "Observed Zinc (mg/day)",
#     "Predicted Zinc (mg/day)"
#   )
# ==============================================================================
train_survey_glm <- function(formula, model_label, x_label, y_label, 
                             design = survey_design) {
  
  # Check required packages are installed
  required_packages <- c("gt", "dplyr", "tibble", "ggplot2", "broom", 
                         "survey", "jtools", "prettyunits")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Train model with specified formula
  model <- survey::svyglm(
    formula = formula,
    design = design,
    na.action = na.omit
  )
  
  # Generate model summary
  summary_output <- jtools::summ(model, model.coefs = FALSE)
  print(summary_output)
  
  # Extract and format model coefficients
  coefficients <- broom::tidy(model) |>
    dplyr::mutate(p.value = prettyunits::pretty_p_value(p.value)) |>
    gt::gt() |>
    gt::fmt_number()
  print(coefficients)
  
  # Generate augmented model results
  model_results <- broom::augment(model) |>
    dplyr::mutate(
      .se.fit = sqrt(attr(.fitted, "var")),
      .fitted = as.numeric(.fitted)
    )
  
  # Create diagnostic plot: observed vs predicted
  plot_out <- model_results |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[all.vars(formula)[1]]], 
                                 y = .fitted)) +
    ggplot2::geom_point(alpha = 0.2) +
    # Perfect prediction line (1:1 reference)
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, face = "bold.italic", 
                                         hjust = 0, color = "grey40"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 1, color = "grey40"),
      axis.line = ggplot2::element_line(color = "grey40"),
      axis.title = ggplot2::element_text(size = 10, color = "grey40", hjust = 1),
      axis.text = ggplot2::element_text(size = 10, color = "grey40"),
      legend.title = ggplot2::element_text(size = 13, face = "bold", 
                                           color = "grey40"),
      legend.text = ggplot2::element_text(size = 11, color = "grey40")
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label
    )
  
  print(plot_out)
  
  return(list(
    model = model,
    coefficients = coefficients,
    results = model_results,
    plot = plot_out
  ))
}

# ==============================================================================
# Train Survey-Weighted Logistic Regression Model
# ==============================================================================
# Trains a survey-weighted logistic regression model (typically for binary
# outcomes like stunting), generates diagnostics, and creates visualization
# of predicted probabilities by outcome group.
#
# Arguments:
#   formula - Model formula (e.g., stunting ~ predictor1 + predictor2)
#   model_label - Descriptive label for the model (used in plot title)
#   x_label - Label for x-axis in diagnostic plot
#   y_label - Label for y-axis in diagnostic plot
#   design - Survey design object (default: survey_design from environment)
#   family - GLM family specification (default: quasibinomial())
#
# Returns:
#   List containing:
#     - model: fitted svyglm model object with logistic specification
#     - coefficients: GT table with model coefficients and p-values
#     - results: augmented data frame with predicted probabilities
#     - plot: ggplot object showing distribution of predictions by outcome
#
# Example:
#   stunting_model <- train_logistic_model(
#     hfa ~ zinc_intake + iron_intake + age,
#     "Stunting Risk Model",
#     "Predicted Probability",
#     "Stunting Status"
#   )
# ==============================================================================
train_logistic_model <- function(formula, model_label, x_label, y_label,
                                 design = survey_design, 
                                 family = quasibinomial()) {
  
  # Check required packages are installed
  required_packages <- c("gt", "dplyr", "tibble", "ggplot2", "broom", 
                         "survey", "jtools", "prettyunits")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Train logistic model with specified formula
  model <- survey::svyglm(
    formula = formula,
    design = design,
    family = family,
    na.action = na.omit
  )
  
  # Generate model summary
  summary_output <- jtools::summ(model, model.coefs = FALSE)
  print(summary_output)
  
  # Extract and format model coefficients
  coefficients <- broom::tidy(model) |>
    dplyr::mutate(p.value = prettyunits::pretty_p_value(p.value)) |>
    gt::gt() |>
    gt::fmt_number()
  print(coefficients)
  
  # Generate augmented model results with predicted probabilities
  model_results <- broom::augment(model, type.predict = "response") |>
    dplyr::mutate(
      .se.fit = sqrt(attr(.fitted, "var")),
      .fitted = as.numeric(.fitted)
    )
  
  # Extract outcome variable name from formula
  outcome_var <- all.vars(formula)[1]
  
  # Create diagnostic plot: predicted probabilities by outcome status
  plot_out <- model_results |>
    ggplot2::ggplot(ggplot2::aes(x = .fitted, y = .data[[outcome_var]])) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, face = "bold.italic", 
                                         hjust = 0, color = "grey40"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 1, color = "grey40"),
      axis.line = ggplot2::element_line(color = "grey40"),
      axis.title = ggplot2::element_text(size = 10, color = "grey40", hjust = 1),
      axis.text = ggplot2::element_text(size = 10, color = "grey40"),
      legend.title = ggplot2::element_text(size = 13, face = "bold", 
                                           color = "grey40"),
      legend.text = ggplot2::element_text(size = 11, color = "grey40")
    ) +
    ggplot2::labs(
      title = model_label,
      x = x_label,
      y = y_label
    )
  
  print(plot_out)
  
  return(list(
    model = model,
    coefficients = coefficients,
    results = model_results,
    plot = plot_out
  ))
}

# ==============================================================================
# Train Survey-Weighted Binomial Model with Logit Link
# ==============================================================================
# Trains a survey-weighted binomial regression model with logit link function
# (equivalent to logistic regression), generates diagnostics, and creates
# visualization of predicted probabilities by outcome group.
#
# Arguments:
#   formula - Model formula (e.g., hfa ~ predictor1 + predictor2)
#   model_label - Descriptive label for the model (used in plot title)
#   x_label - Label for x-axis in diagnostic plot
#   y_label - Label for y-axis in diagnostic plot
#   design - Survey design object (default: survey_design from environment)
#
# Returns:
#   List containing:
#     - model: fitted svyglm model object with quasibinomial(logit) family
#     - coefficients: GT table with model coefficients and p-values
#     - results: augmented data frame with predicted probabilities
#     - plot: ggplot object showing distribution of predictions by outcome
#
# Example:
#   hfa_model <- train_binomial_model(
#     hfa ~ zinc_intake + iron_intake + maternal_height,
#     "Stunting Model",
#     "Predicted Probability",
#     "Stunting Status"
#   )
# ==============================================================================
train_binomial_model <- function(formula, model_label, x_label, y_label,
                                 design = survey_design) {
  
  # Check required packages are installed
  required_packages <- c("gt", "dplyr", "tibble", "ggplot2", "broom", 
                         "survey", "jtools", "prettyunits")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Train binomial model with logit link
  model <- survey::svyglm(
    formula = formula,
    design = design,
    family = quasibinomial(link = "logit"),
    na.action = na.omit
  )
  
  # Generate model summary
  summary_output <- jtools::summ(model, model.coefs = FALSE)
  print(summary_output)
  
  # Extract and format model coefficients
  coefficients <- broom::tidy(model) |>
    dplyr::mutate(p.value = prettyunits::pretty_p_value(p.value)) |>
    gt::gt() |>
    gt::fmt_number()
  print(coefficients)
  
  # Generate augmented model results with predicted probabilities
  model_results <- broom::augment(model, type.predict = "response") |>
    dplyr::mutate(
      .se.fit = sqrt(attr(.fitted, "var")),
      .fitted = as.numeric(.fitted)
    )
  
  # Extract outcome variable name from formula
  outcome_var <- all.vars(formula)[1]
  
  # Create diagnostic plot: predicted probabilities by outcome status
  plot_out <- model_results |>
    ggplot2::ggplot(ggplot2::aes(x = .fitted, y = .data[[outcome_var]])) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, face = "bold.italic", 
                                         hjust = 0, color = "grey40"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 1, color = "grey40"),
      axis.line = ggplot2::element_line(color = "grey40"),
      axis.title = ggplot2::element_text(size = 10, color = "grey40", hjust = 1),
      axis.text = ggplot2::element_text(size = 10, color = "grey40"),
      legend.title = ggplot2::element_text(size = 13, face = "bold", 
                                           color = "grey40"),
      legend.text = ggplot2::element_text(size = 11, color = "grey40")
    ) +
    ggplot2::labs(
      title = model_label,
      x = x_label,
      y = y_label
    )
  
  print(plot_out)
  
  return(list(
    model = model,
    coefficients = coefficients,
    results = model_results,
    plot = plot_out
  ))
}

# ==============================================================================
# Train Polynomial Model with Nonlinear Link Function
# ==============================================================================
# Trains a survey-weighted polynomial regression model with square root link
# function (gaussian family), suitable for continuous outcomes with nonlinear
# relationships. Creates diagnostic plot with marginal distributions.
#
# Arguments:
#   formula - Model formula with polynomial terms (e.g., outcome ~ poly(x, 3))
#   model_label - Descriptive label for the model (used in plot)
#   x_label - Label for x-axis in diagnostic plot
#   y_label - Label for y-axis in diagnostic plot
#   design - Survey design object (default: survey_design from environment)
#
# Returns:
#   List containing:
#     - model: fitted svyglm model with gaussian(link = "sqrt") family
#     - coefficients: GT table with model coefficients and p-values
#     - results: augmented data frame with fitted values
#     - plot: ggplot object with marginal distributions (10% sample)
#
# Example:
#   zlen_model <- train_polynomial_model(
#     zlen ~ poly(zinc_intake, 3) + poly(iron_intake, 2) + age,
#     "Height-for-Age Z-score Model",
#     "Observed Z-score",
#     "Predicted Z-score"
#   )
# ==============================================================================
train_polynomial_model <- function(formula, model_label, x_label, y_label,
                                   design = survey_design) {
  
  # Check required packages are installed
  required_packages <- c("gt", "dplyr", "tibble", "ggplot2", "broom", 
                         "survey", "jtools", "prettyunits", "ggExtra")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Train polynomial model with square root link
  model <- survey::svyglm(
    formula = formula,
    design = design,
    family = gaussian(link = "sqrt"),
    na.action = na.omit
  )
  
  # Generate model summary
  summary_output <- jtools::summ(model, model.coefs = FALSE)
  print(summary_output)
  
  # Extract and format model coefficients
  coefficients <- broom::tidy(model) |>
    dplyr::mutate(p.value = prettyunits::pretty_p_value(p.value)) |>
    gt::gt() |>
    gt::fmt_number()
  print(coefficients)
  
  # Generate augmented model results
  model_results <- broom::augment(model) |>
    dplyr::mutate(
      .se.fit = sqrt(attr(.fitted, "var")),
      .fitted = as.numeric(.fitted)
    )
  
  # Extract outcome variable name from formula
  outcome_var <- all.vars(formula)[1]
  
  # Create diagnostic plot: observed vs predicted (10% sample for clarity)
  plot_base <- model_results |>
    # Random sample of 10% of data for visualization clarity
    dplyr::sample_frac(0.1) |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[outcome_var]], y = .fitted)) +
    ggplot2::geom_point(alpha = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, face = "bold.italic", 
                                         hjust = 0, color = "grey40"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 1, color = "grey40"),
      axis.line = ggplot2::element_line(color = "grey40"),
      axis.title = ggplot2::element_text(size = 10, color = "grey40", hjust = 1),
      axis.text = ggplot2::element_text(size = 10, color = "grey40"),
      legend.title = ggplot2::element_text(size = 13, face = "bold", 
                                           color = "grey40"),
      legend.text = ggplot2::element_text(size = 11, color = "grey40")
    ) +
    ggplot2::labs(
      caption = "Random sample of 10% of data",
      x = x_label,
      y = y_label
    )
  
  # Add marginal distributions (densigram)
  plot_out <- ggExtra::ggMarginal(
    plot_base,
    type = "densigram",
    fill = "steelblue1",
    color = "steelblue4",
    alpha = 0.02,
    bins = 40,
    margins = "both"
  )
  
  print(plot_out)
  
  return(list(
    model = model,
    coefficients = coefficients,
    results = model_results,
    plot = plot_out
  ))
}

# ==============================================================================
# Train Gamma Regression Model with Log Link
# ==============================================================================
# Trains a survey-weighted gamma regression model with log link function,
# suitable for continuous positive outcomes with right-skewed distributions
# (e.g., nutrient intake). Uses gaussian model for initial values to improve
# convergence. Creates diagnostic plot with marginal distributions.
#
# Arguments:
#   formula - Model formula (e.g., zinc_intake ~ age + sex + department)
#   model_label - Descriptive label for the model (used in output)
#   x_label - Label for x-axis in diagnostic plot
#   y_label - Label for y-axis in diagnostic plot
#   design - Survey design object (default: survey_design from environment)
#
# Returns:
#   List containing:
#     - model: fitted svyglm model with Gamma(link = "log") family
#     - coefficients: GT table with model coefficients and p-values
#     - results: augmented data frame with fitted values
#     - plot: ggplot object with marginal distributions (10% sample)
#
# Example:
#   zinc_model <- train_gamma_model(
#     zinc_total ~ age + sex + department + household_size,
#     "Total Zinc Intake Model",
#     "Observed Zinc (mg/day)",
#     "Predicted Zinc (mg/day)"
#   )
# ==============================================================================
train_gamma_model <- function(formula, model_label, x_label, y_label,
                              design = survey_design) {
  
  # Check required packages are installed
  required_packages <- c("gt", "dplyr", "tibble", "ggplot2", "broom", 
                         "survey", "jtools", "prettyunits", "ggExtra", 
                         "equatiomatic")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Train gamma model with log link
  # Use gaussian model coefficients as starting values for convergence
  model <- survey::svyglm(
    formula = formula,
    design = design,
    family = Gamma(link = "log"),
    start = as.vector(coef(survey::svyglm(
      formula = formula,
      design = design,
      family = gaussian(link = "log")
    ))),
    na.action = na.omit
  )
  
  # Generate model summary
  summary_output <- jtools::summ(model, model.coefs = FALSE)
  
  # Display model equation
  equatiomatic::extract_eq(
    model,
    wrap = TRUE,
    terms_per_line = 5,
    operator_location = "start"
  )
  
  # Extract and format model coefficients as GT table
  coefficients <- broom::tidy(model) |>
    dplyr::mutate(p.value = prettyunits::pretty_p_value(p.value)) |>
    gt::gt() |>
    gt::tab_header(
      title = paste0(model_label, " - Model Coefficients"),
      subtitle = "Survey-weighted gamma regression with log link"
    ) |>
    gt::cols_label(
      term = "Term",
      estimate = "Estimate",
      std.error = "Std. Error",
      statistic = "t-statistic",
      p.value = "p-value"
    ) |>
    gt::fmt_number(
      columns = c(estimate, std.error, statistic),
      decimals = 4
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(
        columns = p.value,
        rows = p.value == "<0.001"
      )
    ) |>
    gt::tab_options(
      table.font.size = 11,
      heading.title.font.size = 13,
      heading.subtitle.font.size = 10,
      table.width = gt::pct(100)
    )
  
  # Generate augmented model results
  model_results <- broom::augment(model) |>
    dplyr::mutate(
      .se.fit = sqrt(attr(.fitted, "var")),
      .fitted = exp(as.numeric(.fitted))
    )
  
  # Extract outcome variable name from formula
  outcome_var <- all.vars(formula)[1]
  
  # Create diagnostic plot: observed vs predicted (10% sample for clarity)
  plot_base <- model_results |>
    # Random sample of 10% of data for visualization clarity
    dplyr::sample_frac(0.1) |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[outcome_var]], y = .fitted)) +
    ggplot2::geom_point(alpha = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, face = "bold.italic", 
                                         hjust = 0, color = "azure4"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 1, color = "azure4"),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, color = "azure4", hjust = 1),
      axis.text = ggplot2::element_text(size = 10, color = "azure4"),
      legend.title = ggplot2::element_text(size = 13, face = "bold", 
                                           color = "azure4"),
      legend.text = ggplot2::element_text(size = 11, color = "azure4")
    ) +
    ggplot2::labs(
      caption = "Random sample of 10% of data",
      x = x_label,
      y = y_label
    )
  
  # Add marginal distributions (densigram)
  plot_out <- ggExtra::ggMarginal(
    plot_base,
    type = "densigram",
    fill = "steelblue1",
    color = "steelblue4",
    alpha = 0.02,
    margins = "both"
  )
  
  print(summary_output)
  
  return(list(
    model = model,
    coefficients = coefficients,
    results = model_results,
    plot = plot_out
  ))
}

# ==============================================================================
# Train Gamma Regression Model for Plant Food Percentage
# ==============================================================================
# Trains a survey-weighted gamma regression model with identity link function,
# specifically designed for modeling percentage of plant foods in diet. The
# identity link suits an outcome bounded in [0, 1]. Uses gaussian model for
# initial values to improve convergence.
#
# Arguments:
#   formula - Model formula (e.g., plant_food_pct ~ age + sex + department)
#   model_label - Descriptive label for the model (used in output)
#   x_label - Label for x-axis in diagnostic plot
#   y_label - Label for y-axis in diagnostic plot
#   design - Survey design object (default: survey_design from environment)
#
# Returns:
#   List containing:
#     - model: fitted svyglm model with Gamma(link = "identity") family
#     - coefficients: GT table with model coefficients and p-values
#     - results: augmented data frame with fitted values
#     - plot: ggplot object with marginal distributions (10% sample)
#
# Example:
#   plant_model <- train_gamma_model_plant_foods(
#     plant_food_pct ~ age + sex + area + household_size,
#     "Plant Food Percentage Model",
#     "Observed Plant Food %",
#     "Predicted Plant Food %"
#   )
# ==============================================================================
train_gamma_model_plant_foods <- function(formula, model_label, x_label, y_label,
                                          design = survey_design) {
  
  # Check required packages are installed
  required_packages <- c("gt", "dplyr", "tibble", "ggplot2", "broom", 
                         "survey", "jtools", "prettyunits", "ggExtra", 
                         "equatiomatic")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Train gamma model with identity link (suitable for percentages)
  # Use gaussian model coefficients as starting values for convergence
  model <- survey::svyglm(
    formula = formula,
    design = design,
    family = Gamma(link = "identity"),
    start = as.vector(coef(survey::svyglm(
      formula = formula,
      design = design,
      family = gaussian(link = "identity")
    ))),
    na.action = na.omit
  )
  
  # Generate model summary
  summary_output <- jtools::summ(model, model.coefs = FALSE)
  
  # Display model equation
  equatiomatic::extract_eq(
    model,
    wrap = TRUE,
    terms_per_line = 5,
    operator_location = "start"
  )
  
  # Extract and format model coefficients as GT table
  coefficients <- broom::tidy(model) |>
    dplyr::mutate(p.value = prettyunits::pretty_p_value(p.value)) |>
    gt::gt() |>
    gt::tab_header(
      title = paste0(model_label, " - Model Coefficients"),
      subtitle = "Survey-weighted gamma regression with identity link"
    ) |>
    gt::cols_label(
      term = "Term",
      estimate = "Estimate",
      std.error = "Std. Error",
      statistic = "t-statistic",
      p.value = "p-value"
    ) |>
    gt::fmt_number(
      columns = c(estimate, std.error, statistic),
      decimals = 4
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(
        columns = p.value,
        rows = p.value == "<0.001"
      )
    ) |>
    gt::tab_options(
      table.font.size = 11,
      heading.title.font.size = 13,
      heading.subtitle.font.size = 10,
      table.width = gt::pct(100)
    )
  
  # Generate augmented model results
  model_results <- broom::augment(model) |>
    dplyr::mutate(
      .se.fit = sqrt(attr(.fitted, "var")),
      .fitted = as.numeric(.fitted)
    )
  
  # Extract outcome variable name from formula
  outcome_var <- all.vars(formula)[1]
  
  # Create diagnostic plot: observed vs predicted (10% sample for clarity)
  plot_base <- model_results |>
    # Random sample of 10% of data for visualization clarity
    dplyr::sample_frac(0.1) |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[outcome_var]], y = .fitted)) +
    ggplot2::geom_point(alpha = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, face = "bold.italic", 
                                         hjust = 0, color = "azure4"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 1, color = "azure4"),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, color = "azure4", hjust = 1),
      axis.text = ggplot2::element_text(size = 10, color = "azure4"),
      legend.title = ggplot2::element_text(size = 13, face = "bold", 
                                           color = "azure4"),
      legend.text = ggplot2::element_text(size = 11, color = "azure4")
    ) +
    ggplot2::labs(
      caption = "Random sample of 10% of data",
      x = x_label,
      y = y_label
    )
  
  # Add marginal distributions (densigram)
  plot_out <- ggExtra::ggMarginal(
    plot_base,
    type = "densigram",
    fill = "steelblue1",
    color = "steelblue4",
    alpha = 0.02,
    margins = "both"
  )
  
  print(summary_output)
  
  return(list(
    model = model,
    coefficients = coefficients,
    results = model_results,
    plot = plot_out
  ))
}

# ==============================================================================
# Train Gaussian Regression Model (Standard Linear Regression)
# ==============================================================================
# Trains a survey-weighted gaussian (normal) regression model with identity
# link function, equivalent to standard linear regression for continuous
# outcomes. Suitable for approximately normally distributed outcomes like
# Z-scores. Creates diagnostic plot with marginal distributions.
#
# Arguments:
#   formula - Model formula (e.g., zlen ~ age + sex + nutrient_intake)
#   model_label - Descriptive label for the model (used in output)
#   x_label - Label for x-axis in diagnostic plot
#   y_label - Label for y-axis in diagnostic plot
#   design - Survey design object (default: survey_design from environment)
#
# Returns:
#   List containing:
#     - model: fitted svyglm model with gaussian() family (identity link)
#     - coefficients: GT table with model coefficients and p-values
#     - results: augmented data frame with fitted values
#     - plot: ggplot object with marginal distributions (20% sample)
#
# Example:
#   zlen_model <- train_gaussian_model(
#     zlen ~ zinc_intake + iron_intake + maternal_height + age,
#     "Height-for-Age Z-score Model",
#     "Observed Z-score",
#     "Predicted Z-score"
#   )
# ==============================================================================
train_gaussian_model <- function(formula, model_label, x_label, y_label,
                                 design = survey_design) {
  
  # Check required packages are installed
  required_packages <- c("gt", "dplyr", "tibble", "ggplot2", "broom", 
                         "survey", "jtools", "prettyunits", "ggExtra", 
                         "equatiomatic")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Train gaussian model (standard linear regression)
  model <- survey::svyglm(
    formula = formula,
    design = design,
    family = gaussian(),
    na.action = na.omit
  )
  
  # Generate model summary
  summary_output <- jtools::summ(model, model.coefs = FALSE)
  print(summary_output)
  
  # Display model equation
  equatiomatic::extract_eq(
    model,
    wrap = TRUE,
    terms_per_line = 5,
    operator_location = "start"
  )
  
  # Extract and format model coefficients
  coefficients <- broom::tidy(model) |>
    dplyr::mutate(p.value = prettyunits::pretty_p_value(p.value)) |>
    gt::gt() |>
    gt::fmt_number()
  print(coefficients)
  
  # Generate augmented model results
  model_results <- broom::augment(model) |>
    dplyr::mutate(
      .se.fit = sqrt(attr(.fitted, "var")),
      .fitted = as.numeric(.fitted)
    )
  
  # Extract outcome variable name from formula
  outcome_var <- all.vars(formula)[1]
  
  # Create diagnostic plot: observed vs predicted (20% sample for clarity)
  plot_base <- model_results |>
    # Random sample of 20% of data for visualization clarity
    dplyr::sample_frac(0.2) |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[outcome_var]], y = .fitted)) +
    ggplot2::geom_point(alpha = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, face = "bold.italic", 
                                         hjust = 0, color = "azure4"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 1, color = "azure4"),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, color = "azure4", hjust = 1),
      axis.text = ggplot2::element_text(size = 10, color = "azure4"),
      legend.title = ggplot2::element_text(size = 13, face = "bold", 
                                           color = "azure4"),
      legend.text = ggplot2::element_text(size = 11, color = "azure4")
    ) +
    ggplot2::labs(
      caption = "Random sample of 20% of data",
      x = x_label,
      y = y_label
    )
  
  # Add marginal distributions (densigram)
  plot_out <- ggExtra::ggMarginal(
    plot_base,
    type = "densigram",
    fill = "steelblue1",
    color = "steelblue4",
    alpha = 0.02,
    margins = "both"
  )
  
  print(plot_out)
  
  return(list(
    model = model,
    coefficients = coefficients,
    results = model_results,
    plot = plot_out
  ))
}

# ==============================================================================
# Predict from Survey-Weighted GLM
# ==============================================================================
# Generates predictions from a fitted survey-weighted GLM by constructing the
# model matrix explicitly and multiplying it by the coefficient vector. This
# keeps prediction independent of which drop() method is resolved in the
# calling session, since the Matrix package exports its own drop() for sparse
# matrices alongside base::drop().
#
# Arguments:
#   model - A fitted svyglm model object (from survey::svyglm)
#   newdata - Optional data frame with new observations for prediction.
#             If NULL, returns fitted values from the training data.
#             Must contain all predictor variables used in the model formula.
#   type - Type of prediction: "response" (default) returns predictions on the
#          response scale (probabilities for binomial/quasibinomial, values for
#          gaussian). "link" returns predictions on the linear predictor scale.
#
# Returns:
#   Numeric vector of predictions. Length equals nrow(newdata) if provided,
#   otherwise equals number of observations used in model fitting.
#
# Notes:
#   - "response" applies the inverse link of the fitted model: inverse logit
#     for binomial families, exponential for log links, identity for gaussian
#   - Warning issued if newdata contains factor levels not seen during training
#   - Columns in newdata not matching model coefficients are silently dropped
#
# Example:
#   # Predict on training data
#   fitted_probs <- predict_svyglm_manual(model_hfa, type = "response")
#
#   # Predict on new data
#   new_probs <- predict_svyglm_manual(model_hfa, newdata = df_new, type = "response")
#
#   # Get linear predictor (log-odds for logistic)
#   log_odds <- predict_svyglm_manual(model_hfa, newdata = df_new, type = "link")
# ==============================================================================

predict_svyglm_manual <- function(model, newdata = NULL, type = "response") {
  
  # Validate type argument
  if (!type %in% c("response", "link")) {
    stop("type must be 'response' or 'link'")
  }
  
  # If no newdata provided, return fitted values from model
  if (is.null(newdata)) {
    if (type == "response") {
      return(stats::fitted(model))
    } else {
      return(model$linear.predictors)
    }
  }
  
  # Extract model coefficients and their names
  coefs <- stats::coef(model)
  coef_names <- names(coefs)
  
  # Extract formula without response variable
  formula_rhs <- stats::delete.response(stats::terms(model))
  
  # Create model matrix for new data
  mm_new <- stats::model.matrix(formula_rhs, data = newdata)
  
  # Check for column alignment between model matrix and coefficients
  available_cols <- intersect(colnames(mm_new), coef_names)
  
  if (length(available_cols) < length(coef_names)) {
    missing_cols <- setdiff(coef_names, available_cols)
    warning(
      "Missing ", length(missing_cols), " columns in newdata: ",
      paste(utils::head(missing_cols, 5), collapse = ", "),
      if (length(missing_cols) > 5) paste0(" ... and ", length(missing_cols) - 5, " more"),
      ". Predictions may be unreliable."
    )
  }
  
  # Align model matrix columns to match coefficient order
  mm_aligned <- mm_new[, coef_names, drop = FALSE]
  
  # Perform matrix multiplication (force base matrix class to avoid Matrix issues)
  linear_pred <- as.vector(as.matrix(mm_aligned) %*% coefs)
  
  # Transform to response scale if requested
  if (type == "response") {
    # Inverse link taken from the fitted model, covering every family
    return(stats::family(model)$linkinv(linear_pred))
  }
  
  # Return linear predictor
  return(linear_pred)
}

# ==============================================================================
# Determine Poly Degree
# ==============================================================================
# Infer polynomial degree (1, 2 or 3) for a continuous predictor by fitting
# a univariate GAM against the outcome and translating its effective degrees
# of freedom (edf) into the nearest admissible polynomial degree.
#
# Arguments:
#   data:        data.frame containing the predictor and the outcome
#   predictor:   character, name of the predictor column
#   outcome:     character, name of the outcome column
#   weights:     numeric vector of survey weights (length = nrow(data))
#                OR NULL for unweighted fit
#   family:      gam family (gaussian(), binomial("logit"), ...)
#   k:           basis dimension for s(); default 10, safe for ~750 obs
#   edf_cuts:    numeric vector of length 2 defining grade boundaries:
#                  [0, edf_cuts[1])      -> degree 1 (linear)
#                  [edf_cuts[1], edf_cuts[2]) -> degree 2
#                  [edf_cuts[2], +Inf)   -> degree 3
#                Default: c(1.3, 2.2).
#
# Returns:
#   A named list with components:
#     degree:  integer in {1, 2, 3}. On fit failure, returns 1 with warning.
#     edf:     numeric edf of the smooth term, or NA_real_ on failure.
#     status:  "ok" | "fit_failed" | "edf_unavailable"
#     reason:  character. On failure, the condition message.
#
# Design notes:
#   - The univariate smooth is fitted with mgcv::gam, weighted when survey
#     weights are supplied.
#   - Errors are trapped: any univariate failure falls back to degree 1
#     without aborting the caller. The caller logs the fallback for audit.
#   - Categorical / non-numeric predictors are outside the scope of this
#     function; callers must filter to numeric predictors upstream.
#
# Example:
#   res <- determine_poly_degree(
#     data = df_model,
#     predictor = "cintura_cm",
#     outcome = "zlen",
#     weights = df_model$pesohogar,
#     family = gaussian()
#   )
#   res$degree  # 1, 2, or 3
# ==============================================================================

determine_poly_degree <- function(data,
                                  predictor,
                                  outcome,
                                  weights = NULL,
                                  family = gaussian(),
                                  k = 10,
                                  edf_cuts = c(1.3, 2.2)) {
  stopifnot_env <- function() {
    if (!is.data.frame(data)) {
      stop("'data' must be a data.frame")
    }
    if (!predictor %in% names(data)) {
      stop("Predictor '", predictor, "' not found in data")
    }
    if (!outcome %in% names(data)) {
      stop("Outcome '", outcome, "' not found in data")
    }
    if (!is.numeric(data[[predictor]])) {
      stop("Predictor '", predictor,
           "' is not numeric; determine_poly_degree only applies to numerics")
    }
    if (length(edf_cuts) != 2 || edf_cuts[1] >= edf_cuts[2]) {
      stop("'edf_cuts' must be a length-2 numeric with edf_cuts[1] < edf_cuts[2]")
    }
  }
  stopifnot_env()
  
  # Build formula dynamically: y ~ s(x, k = k, bs = 'tp')
  gam_formula <- as.formula(
    paste0(outcome, " ~ s(", predictor, ", k = ", k, ", bs = 'tp')")
  )
  
  fit_result <- tryCatch(
    {
      fit <- mgcv::gam(
        formula = gam_formula,
        data    = data,
        weights = weights,
        family  = family,
        method  = "REML"
      )
      # Extract edf of the single smooth term
      sm_edf <- sum(fit$edf[grepl(paste0("s\\(", predictor), names(fit$edf))])
      if (!is.finite(sm_edf)) {
        return(list(
          degree = 1L,
          edf    = NA_real_,
          status = "edf_unavailable",
          reason = "edf is not finite"
        ))
      }
      # Translate edf to admissible polynomial degree in {1, 2, 3}
      degree <- dplyr::case_when(
        sm_edf <  edf_cuts[1]                         ~ 1L,
        sm_edf >= edf_cuts[1] & sm_edf < edf_cuts[2]  ~ 2L,
        sm_edf >= edf_cuts[2]                         ~ 3L
      )
      list(
        degree = as.integer(degree),
        edf    = as.numeric(sm_edf),
        status = "ok",
        reason = NA_character_
      )
    },
    error = function(e) {
      list(
        degree = 1L,
        edf    = NA_real_,
        status = "fit_failed",
        reason = conditionMessage(e)
      )
    }
  )
  
  fit_result
}

# ==============================================================================
# Extract Performance Metrics from a Gamma GLM
# ==============================================================================
# Extracts performance metrics from a fitted survey-weighted gamma regression
# (svyglm object) and returns them as a tidy tibble suitable for direct
# rendering as a GT table. Designed to be called from the methodological
# web to populate model performance tables without re-fitting models: the
# .rds artefacts saved in 01_data/02_processed/models/ contain only the
# fitted svyglm objects, and this function turns them back into tabular
# performance summaries on demand.
#
# Pseudo-R² is computed using McFadden's formulation:
#   pseudo_r2_mcfadden = 1 - (residual deviance / null deviance)
# This is well defined for both log-link and identity-link gamma GLMs and
# is the most commonly reported pseudo-R² for GLMs in the ADI framework.
# Values above 0.2 are conventionally considered good for GLMs.
#
# Notes on svyglm method dispatch:
#   - AIC.svyglm() returns a named numeric vector with components
#     'eff.p', 'AIC', 'deltabar'. We extract the "AIC" entry.
#   - BIC.svyglm() requires a "maximal" reference model argument and is
#     therefore excluded from this function.
#   - summary(svyglm)$dispersion returns a 1x2 matrix with columns
#     'variance' and 'SE'. We extract the variance estimate.
#   - The function requires the survey package to be loaded (attached) in
#     the calling environment so that the svyglm S3 methods dispatch.
#
# Arguments:
#   model - Fitted svyglm (or glm) object with deviance and null.deviance
#           components. Typically loaded from an .rds in models/.
#   label - Character string, descriptive label for the model. Used as the
#           identifier row in the returned tibble (e.g. "Iron from maize").
#
# Returns:
#   Tibble with one row containing:
#     - source: descriptive label (as provided)
#     - n_obs: number of observations used to fit the model
#     - n_predictors: number of predictor terms (excluding intercept)
#     - pseudo_r2_mcfadden: McFadden's Pseudo-R²
#     - aic: Akaike Information Criterion
#     - dispersion: estimated dispersion parameter (variance component)
#
# Example:
#   models <- rio::import(
#     here::here("01_data", "02_processed", "models", "03_02_iron_models.rds"),
#     trust = TRUE
#   )
#   dplyr::bind_rows(
#     extract_gamma_model_metrics(models$model_fe_maize,     "Iron from maize"),
#     extract_gamma_model_metrics(models$model_fe_non_maize, "Iron from non-maize")
#   )
# ==============================================================================
extract_gamma_model_metrics <- function(model, label) {
  
  # Check required packages are installed
  required_packages <- c("tibble", "stats", "performance")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  tryCatch({
    # AIC.svyglm returns a named numeric vector; AIC.glm returns a scalar.
    # Extract by name when available, fall back to scalar coercion otherwise.
    aic_raw <- stats::AIC(model)
    aic_value <- if (!is.null(names(aic_raw)) && "AIC" %in% names(aic_raw)) {
      unname(aic_raw["AIC"])
    } else {
      as.numeric(aic_raw)
    }
    
    # summary(svyglm)$dispersion returns a 1x2 matrix; summary(glm)$dispersion
    # returns a scalar. Extract the variance estimate when it's a matrix.
    dispersion_raw <- summary(model)$dispersion
    dispersion_value <- if (is.matrix(dispersion_raw)) {
      unname(dispersion_raw[1, "variance"])
    } else {
      as.numeric(dispersion_raw)
    }
    
    # Cragg-Uhler (Nagelkerke) Pseudo-R². Computed via performance package
    # which handles svyglm dispatch correctly.
    cragg_uhler <- tryCatch(
      as.numeric(performance::r2_nagelkerke(model)),
      error = function(e) NA_real_
    )
    
    tibble::tibble(
      source             = label,
      n_obs              = stats::nobs(model),
      n_predictors       = length(stats::coef(model)) - 1L,
      pseudo_r2_mcfadden = 1 - (model$deviance / model$null.deviance),
      pseudo_r2_cragg_uhler   = cragg_uhler,
      aic                = aic_value,
      dispersion         = dispersion_value
    )
  }, error = function(e) {
    message(paste("Error extracting metrics for model:", label, "\n", e))
    tibble::tibble(
      source             = label,
      n_obs              = NA_integer_,
      n_predictors       = NA_integer_,
      pseudo_r2_mcfadden = NA_real_,
      pseudo_r2_cragg_uhler   = NA_real_,
      aic                = NA_real_,
      dispersion         = NA_real_
    )
  })
}

# ==============================================================================
# 2. DATA VISUALIZATION (MODELS & EFFECTS)
# ==============================================================================

# ==============================================================================
# Determine Best-Fitting Distribution for Outcome Variable
# ==============================================================================
# Compares gamma, lognormal, and exponential distributions to determine which
# best fits the outcome variable. Uses multiple goodness-of-fit tests and
# information criteria. Creates diagnostic visualizations.
#
# Arguments:
#   outcome - Numeric vector with outcome values to analyze
#
# Returns:
#   Invisibly returns NULL. Function prints:
#     - GT table with fit statistics (AIC, BIC, KS, CVM, AD)
#     - 2x2 plot grid with: density comparison, Q-Q plot, CDF comparison, P-P plot
#
# Example:
#   determine_distribution(zinc_intake_data, "Zinc Intake Distribution")
# ==============================================================================
determine_distribution <- function(outcome) {
  
  # Check required packages are installed
  required_packages <- c("ggplot2", "gt", "fitdistrplus", "survey", "ggpubr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Fit candidate distributions
  fit_gamma <- fitdistrplus::fitdist(data = outcome, "gamma")
  fit_lognormal <- fitdistrplus::fitdist(data = outcome, "lnorm")
  fit_exponential <- fitdistrplus::fitdist(data = outcome, "exp")
  
  # Create results data frame with fit statistics
  results_df <- data.frame(
    distribution = c("Gamma", "Lognormal", "Exponential"),
    loglik = c(fit_gamma$loglik, fit_lognormal$loglik, fit_exponential$loglik),
    aic = c(fit_gamma$aic, fit_lognormal$aic, fit_exponential$aic),
    bic = c(fit_gamma$bic, fit_lognormal$bic, fit_exponential$bic),
    ks_stat = c(
      fitdistrplus::gofstat(fit_gamma, fitnames = "gamma")$ks,
      fitdistrplus::gofstat(fit_lognormal, fitnames = "lnorm")$ks,
      fitdistrplus::gofstat(fit_exponential, fitnames = "exp")$ks
    ),
    cvm_stat = c(
      fitdistrplus::gofstat(fit_gamma, fitnames = "gamma")$cvm,
      fitdistrplus::gofstat(fit_lognormal, fitnames = "lnorm")$cvm,
      fitdistrplus::gofstat(fit_exponential, fitnames = "exp")$cvm
    ),
    ad_stat = c(
      fitdistrplus::gofstat(fit_gamma, fitnames = "gamma")$ad,
      fitdistrplus::gofstat(fit_lognormal, fitnames = "lnorm")$ad,
      fitdistrplus::gofstat(fit_exponential, fitnames = "exp")$ad
    )
  )
  
  # Format results as GT table
  fit_table <- results_df |>
    gt::gt() |>
    gt::tab_header(
      title = "Distribution Fit Comparison",
      subtitle = "Goodness-of-fit statistics for Gamma, Lognormal, and Exponential distributions"
    ) |>
    gt::fmt_number(
      columns = c("loglik", "aic", "bic", "ks_stat", "cvm_stat", "ad_stat"),
      decimals = 4
    ) |>
    gt::cols_label(
      distribution = "Distribution",
      loglik = "Log-Likelihood",
      aic = "AIC",
      bic = "BIC",
      ks_stat = "Kolmogorov-Smirnov",
      cvm_stat = "Cramer-von Mises",
      ad_stat = "Anderson-Darling"
    ) |>
    gt::tab_spanner(
      label = "Information Criteria",
      columns = c("aic", "bic")
    ) |>
    gt::tab_spanner(
      label = "Goodness-of-Fit Statistics",
      columns = c("ks_stat", "cvm_stat", "ad_stat")
    ) |>
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_title(groups = "title")
    )
  
  print(fit_table)
  
  # ===========================================================================
  # Create diagnostic plots
  # ===========================================================================
  
  # Plot 1: Empirical vs theoretical density
  plot_density <- data.frame(x = fit_gamma$data) |>
    ggplot2::ggplot(ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = after_stat(density)),
      bins = 30,
      color = "azure4",
      fill = "lightgray",
      alpha = 0.5
    ) +
    ggplot2::stat_function(
      fun = dgamma,
      args = list(
        shape = fit_gamma$estimate["shape"],
        rate = fit_gamma$estimate["rate"]
      ),
      color = "#3AAF85",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Empirical vs Theoretical Density",
      x = "",
      y = "Density"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 10),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # Plot 2: Q-Q plot
  plot_qq <- data.frame(
    empirical = sort(fit_gamma$data),
    theoretical = stats::qgamma(
      stats::ppoints(length(fit_gamma$data)),
      shape = fit_gamma$estimate["shape"],
      rate = fit_gamma$estimate["rate"]
    )
  ) |>
    ggplot2::ggplot(ggplot2::aes(x = theoretical, y = empirical)) +
    ggplot2::geom_point(color = "#1B6CA8", size = 1, alpha = 0.2) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      color = "#3AAF85",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Quantile-Quantile Plot",
      x = "Theoretical Quantiles",
      y = "Empirical Quantiles"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # Plot 3: Empirical vs theoretical CDF
  plot_cdf <- data.frame(
    x = sort(fit_gamma$data),
    empirical_cdf = stats::ecdf(fit_gamma$data)(sort(fit_gamma$data)),
    theoretical_cdf = stats::pgamma(
      sort(fit_gamma$data),
      shape = fit_gamma$estimate["shape"],
      rate = fit_gamma$estimate["rate"]
    )
  ) |>
    ggplot2::ggplot(ggplot2::aes(x = x)) +
    ggplot2::geom_line(ggplot2::aes(y = empirical_cdf),
                       color = "lightgray",
                       size = 2.5) +
    ggplot2::geom_line(ggplot2::aes(y = theoretical_cdf),
                       color = "#3AAF85",
                       linetype = "dashed",
                       size = 0.8) +
    ggplot2::labs(
      title = "Empirical vs Theoretical CDF",
      subtitle = "CDF: Cumulative Distribution Function",
      x = "Data",
      y = "CDF"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 10),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # Plot 4: P-P plot
  plot_pp <- data.frame(
    empirical_prob = stats::ecdf(fit_gamma$data)(sort(fit_gamma$data)),
    theoretical_prob = stats::pgamma(
      sort(fit_gamma$data),
      shape = fit_gamma$estimate["shape"],
      rate = fit_gamma$estimate["rate"]
    )
  ) |>
    ggplot2::ggplot(ggplot2::aes(x = theoretical_prob, y = empirical_prob)) +
    ggplot2::geom_point(color = "#1B6CA8", size = 1, alpha = 0.2) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      color = "#3AAF85",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "P-P Plot",
      x = "Theoretical Probabilities",
      y = "Empirical Probabilities"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # Combine plots into 2x2 grid
  plot_grid <- ggpubr::ggarrange(
    plot_density,
    plot_qq,
    plot_cdf,
    plot_pp,
    ncol = 2, nrow = 2
  )
  
  print(plot_grid)
  
  invisible(NULL)
}

# ==============================================================================
# Determine Best-Fitting Distribution for Energy Outcome Variables
# ==============================================================================
# Compares gamma and lognormal distributions for energy variables (kcal/day).
# Uses method of moments starting values to improve gamma distribution
# convergence with the large numerical values typical of energy data.
#
# Arguments:
#   outcome - Numeric vector with energy values to analyze (kcal/day)
#
# Returns:
#   Invisibly returns NULL. Function prints:
#     - GT table with fit statistics (AIC, BIC, KS, CVM, AD)
#     - 2x2 plot grid with: density comparison, Q-Q plot, CDF comparison, P-P plot
#
# Example:
#   determine_distribution_energy(survey_design$variables$kcal_individuo_dia_maiz)
# ==============================================================================
determine_distribution_energy <- function(outcome) {
  
  # Check required packages are installed
  required_packages <- c("ggplot2", "gt", "fitdistrplus", "survey", "ggpubr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Calculate method of moments starting values for gamma
  shape_start <- mean(outcome, na.rm = TRUE)^2 / var(outcome, na.rm = TRUE)
  rate_start <- mean(outcome, na.rm = TRUE) / var(outcome, na.rm = TRUE)
  
  # Fit candidate distributions (gamma and lognormal only)
  fit_gamma <- fitdistrplus::fitdist(
    data = outcome, 
    "gamma",
    start = list(shape = shape_start, rate = rate_start),
    lower = c(0.01, 0.0001)
  )
  fit_lognormal <- fitdistrplus::fitdist(data = outcome, "lnorm")
  
  # Create results data frame with fit statistics
  results_df <- data.frame(
    distribution = c("Gamma", "Lognormal"),
    loglik = c(fit_gamma$loglik, fit_lognormal$loglik),
    aic = c(fit_gamma$aic, fit_lognormal$aic),
    bic = c(fit_gamma$bic, fit_lognormal$bic),
    ks_stat = c(
      fitdistrplus::gofstat(fit_gamma, fitnames = "gamma")$ks,
      fitdistrplus::gofstat(fit_lognormal, fitnames = "lnorm")$ks
    ),
    cvm_stat = c(
      fitdistrplus::gofstat(fit_gamma, fitnames = "gamma")$cvm,
      fitdistrplus::gofstat(fit_lognormal, fitnames = "lnorm")$cvm
    ),
    ad_stat = c(
      fitdistrplus::gofstat(fit_gamma, fitnames = "gamma")$ad,
      fitdistrplus::gofstat(fit_lognormal, fitnames = "lnorm")$ad
    )
  )
  
  # Format results as GT table
  fit_table <- results_df |>
    gt::gt() |>
    gt::tab_header(
      title = "Distribution Fit Comparison",
      subtitle = "Goodness-of-fit statistics for Gamma and Lognormal distributions"
    ) |>
    gt::fmt_number(
      columns = c("loglik", "aic", "bic", "ks_stat", "cvm_stat", "ad_stat"),
      decimals = 4
    ) |>
    gt::cols_label(
      distribution = "Distribution",
      loglik = "Log-Likelihood",
      aic = "AIC",
      bic = "BIC",
      ks_stat = "Kolmogorov-Smirnov",
      cvm_stat = "Cramer-von Mises",
      ad_stat = "Anderson-Darling"
    ) |>
    gt::tab_spanner(
      label = "Information Criteria",
      columns = c("aic", "bic")
    ) |>
    gt::tab_spanner(
      label = "Goodness-of-Fit Statistics",
      columns = c("ks_stat", "cvm_stat", "ad_stat")
    ) |>
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_title(groups = "title")
    )
  
  print(fit_table)
  
  # ===========================================================================
  # Create diagnostic plots
  # ===========================================================================
  
  # Plot 1: Empirical vs theoretical density
  plot_density <- data.frame(x = fit_gamma$data) |>
    ggplot2::ggplot(ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = after_stat(density)),
      bins = 30,
      color = "azure4",
      fill = "lightgray",
      alpha = 0.5
    ) +
    ggplot2::stat_function(
      fun = dgamma,
      args = list(
        shape = fit_gamma$estimate["shape"],
        rate = fit_gamma$estimate["rate"]
      ),
      color = "#3AAF85",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Empirical vs Theoretical Density",
      x = "",
      y = "Density"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 10),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # Plot 2: Q-Q plot
  plot_qq <- data.frame(
    empirical = sort(fit_gamma$data),
    theoretical = stats::qgamma(
      stats::ppoints(length(fit_gamma$data)),
      shape = fit_gamma$estimate["shape"],
      rate = fit_gamma$estimate["rate"]
    )
  ) |>
    ggplot2::ggplot(ggplot2::aes(x = theoretical, y = empirical)) +
    ggplot2::geom_point(color = "#1B6CA8", size = 1, alpha = 0.2) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      color = "#3AAF85",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Quantile-Quantile Plot",
      x = "Theoretical Quantiles",
      y = "Empirical Quantiles"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # Plot 3: Empirical vs theoretical CDF
  plot_cdf <- data.frame(
    x = sort(fit_gamma$data),
    empirical_cdf = stats::ecdf(fit_gamma$data)(sort(fit_gamma$data)),
    theoretical_cdf = stats::pgamma(
      sort(fit_gamma$data),
      shape = fit_gamma$estimate["shape"],
      rate = fit_gamma$estimate["rate"]
    )
  ) |>
    ggplot2::ggplot(ggplot2::aes(x = x)) +
    ggplot2::geom_line(ggplot2::aes(y = empirical_cdf),
                       color = "lightgray",
                       linewidth = 2.5) +
    ggplot2::geom_line(ggplot2::aes(y = theoretical_cdf),
                       color = "#3AAF85",
                       linetype = "dashed",
                       linewidth = 0.8) +
    ggplot2::labs(
      title = "Empirical vs Theoretical CDF",
      subtitle = "CDF: Cumulative Distribution Function",
      x = "Data",
      y = "CDF"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 10),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # Plot 4: P-P plot
  plot_pp <- data.frame(
    empirical_prob = stats::ecdf(fit_gamma$data)(sort(fit_gamma$data)),
    theoretical_prob = stats::pgamma(
      sort(fit_gamma$data),
      shape = fit_gamma$estimate["shape"],
      rate = fit_gamma$estimate["rate"]
    )
  ) |>
    ggplot2::ggplot(ggplot2::aes(x = theoretical_prob, y = empirical_prob)) +
    ggplot2::geom_point(color = "#1B6CA8", size = 1, alpha = 0.2) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      color = "#3AAF85",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "P-P Plot",
      x = "Theoretical Probabilities",
      y = "Empirical Probabilities"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # Combine plots into 2x2 grid
  plot_grid <- ggpubr::ggarrange(
    plot_density,
    plot_qq,
    plot_cdf,
    plot_pp,
    ncol = 2, nrow = 2
  )
  
  print(plot_grid)
  
  invisible(NULL)
}

# ==============================================================================
# Collinearity Table on the Single-Coefficient VIF Scale
# ==============================================================================
# Returns the collinearity diagnostics of a fitted model with the inflation
# factors expressed on the scale the conventional 5 and 10 thresholds refer to.
#
# performance::check_collinearity() reports the generalized variance inflation
# factor (GVIF) in its VIF column. GVIF grows with the number of model-matrix
# columns a term contributes, so for factors and polynomial terms it is not
# comparable to the thresholds defined for single-coefficient VIF. Raising it
# to the power 1/Df removes that dependence and returns the quantity
# car::vif() reports as the square of GVIF^(1/(2*Df)). Terms contributing a
# single column are unchanged.
#
# Arguments:
#   model - Fitted model object accepted by performance::check_collinearity()
#
# Returns:
#   Data frame from performance::check_collinearity() with an added Df column
#   and VIF, VIF_CI_low and VIF_CI_high rescaled to the single-coefficient VIF
#   scale.
#
# Example:
#   adjust_collinearity_df(zinc_model$model)
# ==============================================================================
adjust_collinearity_df <- function(model) {

  vif_data <- performance::check_collinearity(model)

  # Degrees of freedom per term: how many model-matrix columns it contributes
  assign_index <- attr(stats::model.matrix(model), "assign")
  term_labels  <- attr(stats::terms(model), "term.labels")

  term_df <- as.integer(table(factor(
    assign_index[assign_index > 0],
    levels = seq_along(term_labels)
  )))
  names(term_df) <- term_labels

  df_vec <- term_df[as.character(vif_data$Term)]
  # Terms absent from the model matrix mapping keep the single-column scale
  df_vec[is.na(df_vec)] <- 1L

  vif_data |>
    dplyr::mutate(
      Df          = as.integer(df_vec),
      VIF         = VIF^(1 / df_vec),
      VIF_CI_low  = VIF_CI_low^(1 / df_vec),
      VIF_CI_high = VIF_CI_high^(1 / df_vec)
    )
}

# ==============================================================================
# Evaluate Gamma Regression Model Diagnostics
# ==============================================================================
# Performs diagnostic evaluation of gamma regression models through visual
# inspection of model assumptions. Creates 2x2 grid with:
# residual linearity, homoscedasticity, influential observations, and
# multicollinearity (VIF) plots.
#
# Arguments:
#   model - Fitted gamma regression model object (svyglm with Gamma family)
#
# Returns:
#   Invisibly returns NULL. Function prints 2x2 diagnostic plot grid with:
#     - Residual linearity plot (residuals vs fitted)
#     - Homoscedasticity plot (sqrt standardized residuals vs fitted)
#     - Influential observations plot (leverage vs standardized residuals)
#     - Variance Inflation Factor (VIF) plot for collinearity
#
# Example:
#   zinc_model <- train_gamma_model(zinc_total ~ age + sex + department, ...)
#   evaluate_gamma_model(zinc_model$model)
# ==============================================================================
evaluate_gamma_model <- function(model) {
  
  # Check required packages are installed
  required_packages <- c("ggplot2", "performance", "ggpubr", "dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # ===========================================================================
  # Plot 1: Residual Linearity
  # ===========================================================================
  plot_linearity <- model |>
    ggplot2::ggplot(ggplot2::aes(x = as.numeric(.fitted),
                                 y = as.numeric(.resid))) +
    ggplot2::geom_point(alpha = 0.5, size = 1, color = "#1B6CA8") +
    ggplot2::geom_smooth(
      formula = 'y ~ x',
      se = FALSE,
      method = "loess",
      color = "#3AAF85",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Residual Linearity",
      subtitle = "Reference line should be flat and horizontal",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 10),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # ===========================================================================
  # Plot 2: Homoscedasticity
  # ===========================================================================
  plot_homoscedasticity <- model |>
    ggplot2::ggplot(ggplot2::aes(x = as.numeric(.fitted),
                                 y = sqrt(abs(.stdresid)))) +
    ggplot2::geom_point(alpha = 0.5, size = 1, color = "#1B6CA8") +
    ggplot2::geom_smooth(
      formula = 'y ~ x',
      se = FALSE,
      method = "loess",
      color = "#3AAF85",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Homogeneity of Variance",
      subtitle = "Reference line should be flat and horizontal",
      x = "Fitted Values",
      y = expression(sqrt("Standardized Residuals"))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 10),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # ===========================================================================
  # Plot 3: Influential Observations
  # ===========================================================================
  influential_data <- performance::check_model(
    model,
    residual_type = "normal",
    check = "outliers"
  )$INFLUENTIAL
  
  plot_influential <- influential_data |>
    ggplot2::ggplot(ggplot2::aes(x = Hat, y = Std_Residuals)) +
    ggplot2::geom_point(alpha = 0.5, size = 1, color = "#1B6CA8") +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "azure4",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    # Cook's distance contour lines (upper)
    ggplot2::stat_function(
      fun = function(hat) sqrt(0.5 * length(coef(model)) * (1 - hat) / hat),
      xlim = c(0, max(influential_data$Hat)),
      linetype = "dashed",
      linewidth = 0.8,
      color = "#3AAF85"
    ) +
    # Cook's distance contour lines (lower)
    ggplot2::stat_function(
      fun = function(hat) -sqrt(0.5 * length(coef(model)) * (1 - hat) / hat),
      xlim = c(0, max(influential_data$Hat)),
      linetype = "dashed",
      linewidth = 0.8,
      color = "#3AAF85"
    ) +
    ggplot2::labs(
      title = "Influential Observations",
      subtitle = "Points should be within Cook's distance contours",
      x = "Leverage (Hat)",
      y = "Standardized Residuals"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 10),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # ===========================================================================
  # Plot 4: Variance Inflation Factor (VIF) - Collinearity
  # ===========================================================================
  plot_vif <- adjust_collinearity_df(model) |>
    dplyr::mutate(
      Collinearity = factor(
        cut(VIF,
            breaks = c(-Inf, 5, 10, Inf),
            labels = c("Low (< 5)", "Moderate (< 10)", "High (> 10)")
        ),
        levels = c("Low (< 5)", "Moderate (< 10)", "High (> 10)"),
        ordered = TRUE
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Term, VIF), y = VIF, color = Collinearity)) +
    # Background colored regions
    ggplot2::geom_rect(
      aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 5),
      fill = "lightgreen",
      color = "#E6D7DE",
      alpha = 0.02
    ) +
    ggplot2::geom_rect(
      aes(xmin = -Inf, xmax = Inf, ymin = 5, ymax = 10),
      fill = "lightblue",
      color = "#E6D7DE",
      alpha = 0.02
    ) +
    ggplot2::geom_rect(
      aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = Inf),
      fill = "lightcoral",
      color = "#E6D7DE",
      alpha = 0.02
    ) +
    # VIF points and error bars
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(aes(ymin = VIF_CI_low, ymax = VIF_CI_high), width = 0.2) +
    ggplot2::labs(
      title = "Collinearity Analysis (VIF)",
      x = "",
      y = "VIF (Df-adjusted)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 10),
      axis.line.y = ggplot2::element_line(color = "azure4"),
      axis.line.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text.y = ggplot2::element_text(size = 8, colour = "azure4"),
      axis.text.x = ggplot2::element_text(color = "azure4", angle = 45, hjust = 1, size = 7),
      panel.grid = ggplot2::element_line(color = "#E6D7DE"),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8, color = "azure4")
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Low (< 5)" = "springgreen3",
        "Moderate (< 10)" = "steelblue2",
        "High (> 10)" = "tomato3"
      ),
      name = ""
    )
  
  # ===========================================================================
  # Combine plots into 2x2 grid
  # ===========================================================================
  plot_grid <- ggpubr::ggarrange(
    plot_linearity,
    plot_homoscedasticity,
    plot_influential,
    plot_vif,
    ncol = 2, nrow = 2
  )
  
  print(plot_grid)
  
  invisible(NULL)
}

# ==============================================================================
# Evaluate Quasibinomial Regression Model Diagnostics
# ==============================================================================
# Performs diagnostic evaluation of quasibinomial regression models (logistic
# regression for binary outcomes) through visual inspection of model
# assumptions. Creates 2x2 grid with: residual linearity,
# homoscedasticity, influential observations, and multicollinearity (VIF).
#
# Arguments:
#   model - Fitted quasibinomial regression model (svyglm with quasibinomial)
#
# Returns:
#   Invisibly returns NULL. Function prints 2x2 diagnostic plot grid with:
#     - Residual linearity plot (residuals vs fitted)
#     - Homoscedasticity plot (sqrt standardized residuals vs fitted)
#     - Influential observations plot (leverage vs standardized residuals)
#     - Variance Inflation Factor (VIF) plot for collinearity
#
# Example:
#   stunting_model <- train_logistic_model(hfa ~ zinc + iron + age, ...)
#   evaluate_quasibinomial_model(stunting_model$model)
# ==============================================================================
evaluate_quasibinomial_model <- function(model) {
  
  # Check required packages are installed
  required_packages <- c("ggplot2", "performance", "ggpubr", "dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # ===========================================================================
  # Plot 1: Residual Linearity
  # ===========================================================================
  plot_linearity <- model |>
    performance::model_performance() |>
    ggplot2::ggplot(ggplot2::aes(x = .fitted, y = .resid)) +
    ggplot2::geom_point(alpha = 0.5, size = 1, color = "#1B6CA8") +
    ggplot2::geom_smooth(
      formula = 'y ~ x',
      se = FALSE,
      method = "loess",
      color = "#3AAF85",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Residual Linearity",
      subtitle = "Reference line should be flat and horizontal",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 10),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # ===========================================================================
  # Plot 2: Homoscedasticity
  # ===========================================================================
  plot_homoscedasticity <- model |>
    performance::model_performance() |>
    ggplot2::ggplot(ggplot2::aes(x = .fitted, y = sqrt(abs(.stdresid)))) +
    ggplot2::geom_point(alpha = 0.5, size = 1, color = "#1B6CA8") +
    ggplot2::geom_smooth(
      formula = 'y ~ x',
      se = FALSE,
      method = "loess",
      color = "#3AAF85",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Homogeneity of Variance",
      subtitle = "Reference line should be flat and horizontal",
      x = "Fitted Values",
      y = expression(sqrt("Standardized Residuals"))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 10),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # ===========================================================================
  # Plot 3: Influential Observations
  # ===========================================================================
  influential_data <- performance::check_model(model, check = "outliers")$INFLUENTIAL
  
  plot_influential <- influential_data |>
    ggplot2::ggplot(ggplot2::aes(x = Hat, y = Std_Residuals)) +
    ggplot2::geom_point(alpha = 0.5, size = 1, color = "#1B6CA8") +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "azure4",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    # Cook's distance contour lines (upper)
    ggplot2::stat_function(
      fun = function(hat) sqrt(0.5 * length(coef(model)) * (1 - hat) / hat),
      xlim = c(0, max(influential_data$Hat)),
      linetype = "dashed",
      linewidth = 0.8,
      color = "#3AAF85"
    ) +
    # Cook's distance contour lines (lower)
    ggplot2::stat_function(
      fun = function(hat) -sqrt(0.5 * length(coef(model)) * (1 - hat) / hat),
      xlim = c(0, max(influential_data$Hat)),
      linetype = "dashed",
      linewidth = 0.8,
      color = "#3AAF85"
    ) +
    ggplot2::labs(
      title = "Influential Observations",
      subtitle = "Points should be within Cook's distance contours",
      x = "Leverage (Hat)",
      y = "Standardized Residuals"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 10),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # ===========================================================================
  # Plot 4: Variance Inflation Factor (VIF) - Collinearity
  # ===========================================================================
  vif_data <- adjust_collinearity_df(model)

  plot_vif <- vif_data |>
    dplyr::mutate(
      Collinearity = factor(
        cut(VIF,
            breaks = c(-Inf, 5, 10, Inf),
            labels = c("Low (< 5)", "Moderate (< 10)", "High (> 10)"),
            ordered = TRUE
        )
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(Term, VIF), y = VIF, color = Collinearity)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(aes(ymin = VIF_CI_low, ymax = VIF_CI_high), width = 0.2) +
    ggplot2::labs(
      title = "Collinearity (VIF)",
      x = "",
      y = "VIF (Df-adjusted)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "azure4", size = 14),
      axis.title = ggplot2::element_text(size = 10, colour = "azure4"),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4"),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8, colour = "azure4")
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Low (< 5)" = "springgreen3",
        "Moderate (< 10)" = "steelblue2",
        "High (> 10)" = "tomato3"
      ),
      name = ""
    )
  
  # ===========================================================================
  # Combine plots into 2x2 grid
  # ===========================================================================
  plot_grid <- ggpubr::ggarrange(
    plot_linearity,
    plot_homoscedasticity,
    plot_influential,
    plot_vif,
    ncol = 2, nrow = 2
  )
  
  print(plot_grid)
  
  invisible(NULL)
}

# ==============================================================================
# Calculate Classification Metrics Across Thresholds
# ==============================================================================
# Computes precision, recall, and F1-score for binary classification across
# a range of probability thresholds. Used for threshold optimization in
# logistic regression and other binary classifiers.
#
# Arguments:
#   thresholds - Numeric vector of probability thresholds to evaluate (0-1)
#   probs - Numeric vector of predicted probabilities from model
#   labels - Numeric/integer vector of true binary labels (0 or 1)
#
# Returns:
#   Data frame with columns:
#     - threshold: evaluated threshold value
#     - precision: TP / (TP + FP) at this threshold
#     - recall: TP / (TP + FN) at this threshold
#     - f1_score: harmonic mean of precision and recall
#
# Example:
#   thresholds <- seq(0.3, 0.7, by = 0.01)
#   metrics <- calculate_metrics(thresholds, model_probs, true_labels)
#   optimal_threshold <- metrics$threshold[which.max(metrics$f1_score)]
# ==============================================================================
calculate_metrics <- function(thresholds, probs, labels) {
  
  metrics <- data.frame(
    threshold = thresholds,
    precision = numeric(length(thresholds)),
    recall = numeric(length(thresholds)),
    f1_score = numeric(length(thresholds))
  )
  
  # Calculate metrics for each threshold
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    
    # Convert probabilities to binary predictions
    predicted <- ifelse(probs >= threshold, 1, 0)
    
    # Calculate confusion matrix components
    tp <- sum(predicted == 1 & labels == 1)  # True Positives
    fp <- sum(predicted == 1 & labels == 0)  # False Positives
    fn <- sum(predicted == 0 & labels == 1)  # False Negatives
    
    # Calculate precision (avoid division by zero)
    precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
    
    # Calculate recall (avoid division by zero)
    recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
    
    # Calculate F1-score (harmonic mean of precision and recall)
    f1 <- ifelse(
      (precision + recall) > 0,
      2 * precision * recall / (precision + recall),
      0
    )
    
    metrics$precision[i] <- precision
    metrics$recall[i] <- recall
    metrics$f1_score[i] <- f1
  }
  
  return(metrics)
}


# ==============================================================================
# 3. DISTRIBUTION ANALYSIS & COMPARISON
# ==============================================================================

# ==============================================================================
# Plot Model Fit Diagnostics
# ==============================================================================
# Creates diagnostic scatter plot comparing observed vs predicted values with
# perfect prediction reference line (1:1). Useful for evaluating model fit
# quality in regression models.
#
# Arguments:
#   model - Fitted regression model object (svyglm, glm, or similar)
#   x_var - Character string with name of predictor variable for x-axis
#   x_label - Label for x-axis (e.g., "Observed Zinc (mg/day)")
#   y_label - Label for y-axis (e.g., "Predicted Zinc (mg/day)")
#
# Returns:
#   ggplot2 object with scatter plot of observed vs predicted values
#
# Example:
#   zinc_plot <- plot_model_fit(
#     zinc_model$model,
#     "zinc_total",
#     "Observed Zinc (mg/day)",
#     "Predicted Zinc (mg/day)"
#   )
#   print(zinc_plot)
# ==============================================================================
plot_model_fit <- function(model, x_var, x_label, y_label) {
  
  # Check required packages are installed
  required_packages <- c("ggplot2", "broom", "dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Augment model with predictions and standard errors
  model_results <- broom::augment(model) |>
    dplyr::mutate(
      .se.fit = sqrt(attr(.fitted, "var")),
      .fitted = as.numeric(.fitted)
    )
  
  # Create diagnostic plot
  plot_out <- model_results |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[x_var]], y = .fitted)) +
    ggplot2::geom_point(alpha = 0.2) +
    # Perfect prediction reference line (1:1)
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, face = "bold.italic", 
                                         hjust = 0, color = "grey40"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 1, color = "grey40"),
      axis.line = ggplot2::element_line(color = "grey40"),
      axis.title = ggplot2::element_text(size = 10, color = "grey40", hjust = 1),
      axis.text = ggplot2::element_text(size = 10, color = "grey40"),
      legend.title = ggplot2::element_text(size = 13, face = "bold", color = "grey40"),
      legend.text = ggplot2::element_text(size = 11, color = "grey40")
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label
    )
  
  return(plot_out)
}

# ==============================================================================
# Plot Variable Effects in Model
# ==============================================================================
# Creates effect plots showing the relationship between predictor variables
# and the outcome, with confidence intervals. Handles multiple variables by
# iterating and printing each plot. Variable names are automatically formatted
# for display (underscores replaced with spaces, title case).
#
# Arguments:
#   model - Fitted regression model object (svyglm, glm, or similar)
#   variables - Character vector with variable names to plot
#   y_label - Label for y-axis (outcome variable)
#   colors - Color for effect plot (default: "#C44441")
#
# Returns:
#   NULL (invisibly). Function prints plots directly for each variable.
#
# Example:
#   plot_effect(
#     stunting_model$model,
#     c("zinc_intake", "iron_intake", "maternal_height"),
#     "Stunting Probability",
#     colors = "#C44441"
#   )
# ==============================================================================
plot_effect <- function(model, variables, y_label, colors = "#C44441") {
  
  # Check required packages are installed
  required_packages <- c("jtools", "ggplot2", "dplyr", "rlang", "purrr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Iterate over variables and create effect plots
  purrr::iwalk(variables, function(pred, idx) {
    
    # Verify variable exists in model data
    if (!pred %in% names(model$model)) {
      stop(paste("Variable", pred, "not found in model data."))
    }
    
    # Format variable name for display label
    # Replace underscores with spaces and convert to title case
    pred_label <- gsub("_", " ", pred)
    pred_label <- paste0(
      toupper(substring(pred_label, 1, 1)),
      substring(pred_label, 2)
    )
    
    # Generate effect plot
    plot_out <- jtools::effect_plot(
      model,
      pred = !!rlang::sym(pred),
      interval = TRUE,
      x.label = pred_label,
      y.label = y_label,
      colors = colors
    ) +
      # Flip coordinates for better readability
      ggplot2::coord_flip() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 10, color = "grey60", hjust = 0),
        axis.text = ggplot2::element_text(size = 9, color = "grey70")
      )
    
    print(plot_out)
  })
  
  invisible(NULL)
}

# ==============================================================================
# Plot Complex Effect with Age Stratification
# ==============================================================================
# Creates faceted plot showing dose-response relationship between nutrient
# intake and stunting probability, stratified by age groups. Includes
# recommended intake threshold as vertical reference line and polynomial
# smoothing to capture non-linear relationships.
#
# Arguments:
#   data - Data frame with required columns (see below)
#   title - Main plot title
#   x_axis_label - Label for x-axis (nutrient name and units)
#
# Required columns in data:
#   - var_nutricional: numeric, nutrient intake values
#   - prediccion: numeric, predicted probability of stunting (0-1)
#   - edad: factor/character, age group labels
#   - recomendacion: numeric, recommended intake threshold
#
# Returns:
#   ggplot2 object with faceted dose-response plot
#
# Example:
#   effect_data <- data.frame(
#     var_nutricional = rep(seq(0, 15, 0.5), 3),
#     prediccion = c(...),
#     edad = rep(c("6-11m", "12-23m", "24-59m"), each = 31),
#     recomendacion = rep(c(5, 7, 10), each = 31)
#   )
#   plot_complex_effect(effect_data, "Iron Effect on Stunting", "Iron Intake (mg/day)")
# ==============================================================================
plot_complex_effect <- function(data, title, x_axis_label) {
  
  # Check required packages are installed
  required_packages <- c("ggplot2", "scales", "dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Verify required columns exist in data frame
  required_columns <- c("var_nutricional", "prediccion", "edad", "recomendacion")
  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in data frame:", paste(missing_cols, collapse = ", ")))
  }
  
  ggplot2::ggplot(data, ggplot2::aes(x = var_nutricional, y = prediccion)) +
    # Dose-response curve (polynomial smoothing)
    ggplot2::geom_smooth(
      method = "glm",
      formula = y ~ poly(x, 3),
      se = FALSE,
      color = "#C44441",
      linewidth = 1
    ) +
    # Recommended intake threshold (vertical line)
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = recomendacion),
      linetype = "dashed",
      linewidth = 0.8,
      color = "azure4"
    ) +
    # Stratification by age groups
    ggplot2::facet_wrap(~edad, scales = "fixed") +
    ggplot2::labs(
      title = title,
      subtitle = "Results stratified by age groups",
      x = x_axis_label,
      y = "Stunting Probability"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, face = "bold", 
                                         hjust = 0, color = "azure4"),
      plot.subtitle = ggplot2::element_text(size = 11, hjust = 0, color = "azure4"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 1, color = "azure4"),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, color = "azure4", hjust = 1),
      axis.text = ggplot2::element_text(size = 10, color = "azure4"),
      strip.text = ggplot2::element_text(size = 12, face = "italic", color = "azure4")
    ) +
    # Y-axis labels as percentages
    ggplot2::scale_y_continuous(
      breaks = c(0.25, 0.50, 0.75, 1.00),
      labels = scales::percent_format(accuracy = 1)
    )
}

# ==============================================================================
# Plot Linear Effect with Age Stratification
# ==============================================================================
# Creates faceted plot showing linear dose-response relationship between
# nutrient intake and stunting probability, stratified by age groups, with
# linear smoothing. Includes recommended intake threshold as vertical
# reference line.
#
# Arguments:
#   data - Data frame with required columns (see below)
#   title - Main plot title
#   x_axis_label - Label for x-axis (nutrient name and units)
#
# Required columns in data:
#   - var_nutricional: numeric, nutrient intake values
#   - prediccion: numeric, predicted probability of stunting (0-1)
#   - edad: factor/character, age group labels
#   - recomendacion: numeric, recommended intake threshold
#
# Returns:
#   ggplot2 object with faceted linear dose-response plot
#
# Example:
#   effect_data <- data.frame(
#     var_nutricional = rep(seq(0, 15, 0.5), 3),
#     prediccion = c(...),
#     edad = rep(c("6-11m", "12-23m", "24-59m"), each = 31),
#     recomendacion = rep(c(5, 7, 10), each = 31)
#   )
#   plot_linear_effect(effect_data, "Protein Effect on Stunting", "Protein Intake (g/day)")
# ==============================================================================
plot_linear_effect <- function(data, title, x_axis_label) {
  
  # Check required packages are installed
  required_packages <- c("ggplot2", "scales", "dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Verify required columns exist in data frame
  required_columns <- c("var_nutricional", "prediccion", "edad", "recomendacion")
  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in data frame:", paste(missing_cols, collapse = ", ")))
  }
  
  ggplot2::ggplot(data, ggplot2::aes(x = var_nutricional, y = prediccion)) +
    # Linear dose-response line
    ggplot2::geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "#C44441",
      linewidth = 1
    ) +
    # Recommended intake threshold (vertical line)
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = recomendacion),
      linetype = "dashed",
      linewidth = 0.8,
      color = "azure4"
    ) +
    # Stratification by age groups
    ggplot2::facet_wrap(~edad, scales = "fixed") +
    ggplot2::labs(
      title = title,
      subtitle = "Results stratified by age groups",
      x = x_axis_label,
      y = "Stunting Probability"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, face = "bold", 
                                         hjust = 0, color = "azure4"),
      plot.subtitle = ggplot2::element_text(size = 11, hjust = 0, color = "azure4"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 1, color = "azure4"),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 10, color = "azure4", hjust = 1),
      axis.text = ggplot2::element_text(size = 10, color = "azure4"),
      strip.text = ggplot2::element_text(size = 12, face = "italic", color = "azure4")
    ) +
    # Y-axis labels as percentages
    ggplot2::scale_y_continuous(
      breaks = c(0.25, 0.50, 0.75, 1.00),
      labels = scales::percent_format(accuracy = 1)
    )
}

# ==============================================================================
# 4. NUTRIENT CALCULATIONS & ADJUSTMENTS
# ==============================================================================

# ==============================================================================
# Quantile Mapping with Gamma Distribution
# ==============================================================================
# Performs quantile mapping to adjust one distribution to match another using
# gamma distribution parameters. Maps quantiles from source distribution to
# target distribution, preserving rank order while matching target shape.
# Commonly used to calibrate predicted values to match observed distributions.
#
# Arguments:
#   source_distribution - Numeric vector with values to adjust
#   reference_distribution - Numeric vector with target distribution to match
#
# Returns:
#   Numeric vector with adjusted values matching reference distribution shape
#
# Details:
#   1. Fits gamma distribution to both source and reference using method of
#      moments starting values for improved convergence
#   2. Calculates quantiles of source values in source distribution
#   3. Maps those quantiles to corresponding values in reference distribution
#   4. Values <= 0 are replaced with 0.001 to ensure gamma fit compatibility
#
# Example:
#   # Adjust predicted zinc intake to match observed distribution
#   predicted_zinc <- c(5.2, 8.1, 6.3, ...)
#   observed_zinc <- c(4.8, 7.5, 6.1, ...)
#   adjusted_zinc <- quantile_mapping_gamma(predicted_zinc, observed_zinc)
# ==============================================================================
quantile_mapping_gamma <- function(source_distribution, reference_distribution) {
  
  # Check required packages are installed
  required_packages <- c("fitdistrplus", "stats")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Handle zero and negative values (gamma requires positive values)
  # Replace with small positive constant to enable fitting
  source_distribution <- ifelse(source_distribution <= 0, 0.001, source_distribution)
  reference_distribution <- ifelse(reference_distribution <= 0, 0.001, reference_distribution)
  
  # Calculate method of moments starting values for reference distribution
  ref_mean <- mean(reference_distribution, na.rm = TRUE)
  ref_var <- var(reference_distribution, na.rm = TRUE)
  ref_shape_start <- ref_mean^2 / ref_var
  ref_rate_start <- ref_mean / ref_var
  
  # Calculate method of moments starting values for source distribution
  src_mean <- mean(source_distribution, na.rm = TRUE)
  src_var <- var(source_distribution, na.rm = TRUE)
  src_shape_start <- src_mean^2 / src_var
  src_rate_start <- src_mean / src_var
  
  # Fit gamma distribution to reference distribution with MoM starting values
  fit_reference <- suppressWarnings(
    fitdistrplus::fitdist(
      reference_distribution, 
      "gamma",
      start = list(shape = ref_shape_start, rate = ref_rate_start),
      lower = c(0.01, 0.0001)
    )
  )
  
  # Fit gamma distribution to source distribution with MoM starting values
  fit_source <- suppressWarnings(
    fitdistrplus::fitdist(
      source_distribution, 
      "gamma",
      start = list(shape = src_shape_start, rate = src_rate_start),
      lower = c(0.01, 0.0001)
    )
  )
  
  # Calculate quantiles of source values in source distribution
  quantiles_source <- stats::pgamma(
    source_distribution,
    shape = fit_source$estimate["shape"],
    rate = fit_source$estimate["rate"]
  )
  
  # Map quantiles to reference distribution
  # (same quantile positions, but in reference distribution scale)
  adjusted_distribution <- stats::qgamma(
    quantiles_source,
    shape = fit_reference$estimate["shape"],
    rate = fit_reference$estimate["rate"]
  )
  
  return(adjusted_distribution)
}

# ==============================================================================
# Filter Reference Distribution for Quantile Mapping
# ==============================================================================
# Filters a reference distribution by removing zeros and upper outliers using
# Tukey method. This preprocessing is required before quantile mapping with
# gamma distribution, as gamma requires strictly positive values and is
# sensitive to extreme outliers.
#
# Arguments:
#   x - Numeric vector with reference distribution values
#
# Returns:
#   Numeric vector with filtered values (zeros and upper outliers removed)
#
# Details:
#   1. Removes zeros and NA values (gamma requires positive values)
#   2. Calculates upper limit using Tukey method: Q3 + 1.5 * IQR
#   3. Removes values exceeding upper limit
#
# Example:
#   # Filter ENCOVI iron reference before quantile mapping
#   fe_reference_filtered <- filter_reference_distribution(encovi_data$fe_maiz)
#   adjusted_values <- quantile_mapping_gamma(predicted_fe, fe_reference_filtered)
# ==============================================================================
filter_reference_distribution <- function(x) {
  
  # Remove zeros and NA values (incompatible with gamma distribution)
  x_positive <- x[x > 0 & !is.na(x)]
  
  # Calculate upper limit using Tukey method (Q3 + 1.5 * IQR)
  q3 <- stats::quantile(x_positive, 0.75, na.rm = TRUE)
  iqr_value <- stats::IQR(x_positive, na.rm = TRUE)
  upper_limit <- q3 + 1.5 * iqr_value
  
  # Remove upper outliers
  x_filtered <- x_positive[x_positive <= upper_limit]
  
  return(x_filtered)
}

# ==============================================================================
# Rank-Based Nutrient Alignment for Maize Variables
# ==============================================================================
# Realigns individual maize-source nutrient values against the individual's
# overall non-maize nutrient profile, under the assumption of intra-individual
# nutrient coherence.
#
# Arguments:
#   data - Data frame containing nutrient variables for children
#   maize_vars - Character vector of maize-source variable names to adjust
#   nomaize_vars - Character vector of non-maize variable names for anchor index
#   keep_diagnostics - Logical, whether to retain diagnostic columns (default: FALSE)
#
# Returns:
#   Data frame with original data plus adjusted maize variables (suffix "_aligned").
#   If keep_diagnostics = TRUE, also includes:
#     - indice_nomaiz_anchor: composite non-maize nutrient index
#     - rank_nomaiz_anchor: individual ranking by nutrient profile
#
# Details:
#   1. Creates composite non-maize index (standardized mean of all non-maize vars)
#   2. Ranks individuals by index (higher = better nutrient profile)
#   3. Sorts each maize variable values in descending order
#   4. Reassigns: best-ranked individual receives highest maize value
#   5. Preserves marginal distributions exactly (mean, SD, quantiles unchanged)
#
#
# Example:
#   maize_vars <- c("fe_mg_maiz", "zn_mg_maiz", "prot_mg_maiz",
#                   "lys_mg_maiz", "trp_mg_maiz", "ene_kcal_maiz")
#   nomaize_vars <- c("fe_mg_nomaiz", "zn_mg_nomaiz", "prot_mg_nomaiz",
#                     "lys_mg_nomaiz", "trp_mg_nomaiz", "ene_kcal_nomaiz")
#   df_adjusted <- align_maize_nutrition(df_children, maize_vars, nomaize_vars)
# ==============================================================================
align_maize_nutrition <- function(data, maize_vars, nomaize_vars, 
                                  keep_diagnostics = FALSE) {
  
  # Validate required variables exist
  missing_maize <- setdiff(maize_vars, names(data))
  if (length(missing_maize) > 0) {
    stop("Missing maize variables: ", paste(missing_maize, collapse = ", "))
  }
  
  missing_nomaize <- setdiff(nomaize_vars, names(data))
  if (length(missing_nomaize) > 0) {
    stop("Missing non-maize variables: ", paste(missing_nomaize, collapse = ", "))
  }
  
  # Calculate composite non-maize anchor index (standardized mean)
  nomaize_matrix <- as.matrix(data[, nomaize_vars, drop = FALSE])
  nomaize_scaled <- scale(nomaize_matrix)
  indice_nomaiz <- rowMeans(nomaize_scaled, na.rm = TRUE)
  
  # Rank individuals by nutrient profile (higher = better = rank 1)
  rank_indice <- rank(-indice_nomaiz, ties.method = "average", na.last = "keep")
  
  # Apply rank-based alignment to each maize variable
  for (var in maize_vars) {
    original_values <- data[[var]]
    sorted_values <- sort(original_values, decreasing = TRUE, na.last = TRUE)
    aligned_values <- sorted_values[rank_indice]
    data[[paste0(var, "_aligned")]] <- aligned_values
  }
  
  # Add diagnostic columns if requested
  if (keep_diagnostics) {
    data$indice_nomaiz_anchor <- indice_nomaiz
    data$rank_nomaiz_anchor <- rank_indice
  }
  
  return(data)
}

# ==============================================================================
# Compare Synthetic vs Original Data Distributions
# ==============================================================================
# Compares distribution of a single variable between original and synthetic
# datasets using Jensen-Shannon divergence. Handles both numeric (continuous)
# and categorical variables automatically. Creates side-by-side bar charts
# showing frequency distributions.
#
# Arguments:
#   df_original - Data frame with original data
#   df_synthetic - Data frame with synthetic data
#   var_name - Character string with variable name to compare
#   n_bins - Number of bins for discretizing numeric variables (default: 15)
#
# Returns:
#   ggplot2 object with side-by-side comparison bar chart and J-S divergence
#
# Details:
#   For numeric variables: discretizes into bins, compares distributions
#   For categorical variables: compares category frequencies directly
#   Jensen-Shannon divergence ranges [0, 1]: 0 = identical, 1 = completely different
#
# Example:
#   plot <- compare_synthetic_distributions(
#     original_data,
#     synthetic_data,
#     "zinc_intake",
#     n_bins = 20
#   )
# ==============================================================================
compare_synthetic_distributions <- function(df_original, df_synthetic, 
                                            var_name, n_bins = 15) {
  
  # Check required packages are installed
  required_packages <- c("ggplot2", "philentropy", "tidyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Verify variable exists in both data frames
  if (!var_name %in% names(df_original)) {
    stop(paste("Variable", var_name, "does not exist in df_original"))
  }
  if (!var_name %in% names(df_synthetic)) {
    stop(paste("Variable", var_name, "does not exist in df_synthetic"))
  }
  
  # Detect variable type (numeric or categorical)
  var_class <- class(df_original[[var_name]])
  
  # ===========================================================================
  # NUMERIC VARIABLE CASE
  # ===========================================================================
  if (is.numeric(df_original[[var_name]])) {
    
    # Extract variable values (work with copies, not original data frames)
    var_orig <- df_original[[var_name]]
    var_syn <- df_synthetic[[var_name]]
    
    # Determine combined min/max range
    min_val <- min(c(var_orig, var_syn), na.rm = TRUE)
    max_val <- max(c(var_orig, var_syn), na.rm = TRUE)
    
    # Create bins for discretization
    bins <- seq(min_val, max_val, length.out = n_bins)
    
    # Discretize into bins
    orig_cut <- cut(var_orig, bins, include.lowest = TRUE, ordered_result = TRUE)
    syn_cut <- cut(var_syn, bins, include.lowest = TRUE, ordered_result = TRUE)
    
    # Calculate relative frequencies
    tab_orig <- prop.table(table(orig_cut))
    tab_syn <- prop.table(table(syn_cut))
    
    # Calculate Jensen-Shannon divergence
    mat_jsd <- rbind(tab_orig, tab_syn)
    jsd_value <- philentropy::distance(mat_jsd, method = "jensen-shannon", 
                                       mute.message = TRUE)
    
    # Prepare data frame for plotting
    df_plot <- data.frame(
      bin_x = names(tab_orig),
      observed = as.numeric(tab_orig) * 100,
      synthetic = as.numeric(tab_syn) * 100
    ) |>
      tidyr::pivot_longer(
        cols = c("observed", "synthetic"),
        names_to = "type",
        values_to = "percentage"
      )
    
    plot_out <- ggplot2::ggplot(df_plot, 
                                ggplot2::aes(x = bin_x, y = percentage, fill = type)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::scale_fill_manual(
        values = c("observed" = "#1f78b4", "synthetic" = "#a6cee3")
      ) +
      ggplot2::labs(
        x = var_name,
        y = "Percentage",
        fill = "Type",
        title = "Distribution Comparison",
        subtitle = paste0("Jensen-Shannon Divergence: ", 
                          formatC(jsd_value, digits = 3, format = "g"))
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(color = "azure4", size = 15),
        plot.subtitle = ggplot2::element_text(color = "azure4", size = 11),
        axis.line = ggplot2::element_line(color = "azure4"),
        axis.title = ggplot2::element_text(size = 11, colour = "azure4", hjust = 0),
        axis.text = ggplot2::element_text(size = 8, colour = "azure4"),
        legend.title = ggplot2::element_text(size = 11, colour = "azure4"),
        legend.text = ggplot2::element_text(size = 9, colour = "azure4"),
        legend.position = "bottom"
      )
    
    return(plot_out)
    
    # ===========================================================================
    # CATEGORICAL VARIABLE CASE
    # ===========================================================================
  } else {
    
    # Extract variable values (work with copies, not original data frames)
    var_orig <- df_original[[var_name]]
    var_syn <- df_synthetic[[var_name]]
    
    # Convert to factor if character
    if (is.character(var_orig)) {
      var_orig <- as.factor(var_orig)
    }
    if (is.character(var_syn)) {
      var_syn <- as.factor(var_syn)
    }
    
    # Ensure both have same factor levels (union of both)
    all_levels <- union(levels(var_orig), levels(var_syn))
    var_orig <- factor(var_orig, levels = all_levels)
    var_syn <- factor(var_syn, levels = all_levels)
    
    # Calculate relative frequencies
    tab_orig <- prop.table(table(var_orig))
    tab_syn <- prop.table(table(var_syn))
    
    # Calculate Jensen-Shannon divergence
    mat_jsd <- rbind(tab_orig, tab_syn)
    jsd_value <- philentropy::distance(mat_jsd, method = "jensen-shannon", 
                                       mute.message = TRUE)
    
    # Prepare data frame for plotting
    df_plot <- data.frame(
      category = names(tab_orig),
      observed = as.numeric(tab_orig) * 100,
      synthetic = as.numeric(tab_syn) * 100
    ) |>
      tidyr::pivot_longer(
        cols = c("observed", "synthetic"),
        names_to = "type",
        values_to = "percentage"
      )
    
    plot_out <- ggplot2::ggplot(df_plot, 
                                ggplot2::aes(x = category, y = percentage, fill = type)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::scale_fill_manual(
        values = c("observed" = "#1f78b4", "synthetic" = "#a6cee3")
      ) +
      ggplot2::labs(
        x = var_name,
        y = "Percentage",
        fill = "Type",
        title = "Distribution Comparison",
        subtitle = paste0("Jensen-Shannon Divergence: ", 
                          formatC(jsd_value, digits = 3, format = "g"))
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(color = "azure4", size = 15),
        plot.subtitle = ggplot2::element_text(color = "azure4", size = 11),
        axis.line = ggplot2::element_line(color = "azure4"),
        axis.title = ggplot2::element_text(size = 11, colour = "azure4", hjust = 0),
        axis.text.x = ggplot2::element_text(size = 8, colour = "azure4", 
                                            angle = 45, hjust = 1),
        axis.text.y = ggplot2::element_text(size = 8, colour = "azure4"),
        legend.title = ggplot2::element_text(size = 11, colour = "azure4"),
        legend.text = ggplot2::element_text(size = 9, colour = "azure4"),
        legend.position = "bottom"
      )
    
    return(plot_out)
  }
}

# ==============================================================================
# Compare Synthetic Data with Detailed Bivariate Analysis
# ==============================================================================
# Performs detailed comparison of synthetic vs original data using two-way
# Jensen-Shannon divergence analysis: (1) nutrient ~ stunting status, and
# (2) nutrient ~ department. Creates side-by-side raincloud plots showing
# distribution differences. Used for validating synthetic data quality.
#
# Arguments:
#   df_original_hfa - Original data frame with the stunting variable
#   df_original_dept - Original data frame with department variable
#   df_synthetic - Synthetic data frame to validate
#   var_name - Character string with variable name to compare
#
# Returns:
#   Combined ggplot object (via ggpubr::ggarrange) with two comparison plots:
#     - Left: Distribution by stunting status (stunted vs not stunted)
#     - Right: Distribution by department (geographic variation)
#   Each plot includes Jensen-Shannon divergence value in subtitle
#
# Example:
#   comparison_plot <- compare_synthetic_detailed(
#     original_data_hfa,
#     original_data_dept,
#     synthetic_data,
#     "zinc_intake"
#   )
# ==============================================================================
compare_synthetic_detailed <- function(df_original_hfa, df_original_dept,
                                       df_synthetic, var_name) {
  
  # Check required packages are installed
  required_packages <- c("ggplot2", "ggdist", "dplyr", "ggpubr", 
                         "philentropy", "ggtext", "stats")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Define number of bins for discretization
  bins_num <- 10
  
  # ===========================================================================
  # A) COMPARISON BY STUNTING STATUS
  # ===========================================================================
  
  # Calculate J-S divergence for joint distribution "var_name" ~ hfa
  
  # Extract variable values (work with copies)
  var_orig_hfa <- df_original_hfa[[var_name]]
  var_syn_hfa <- df_synthetic[[var_name]]
  
  # Create cut sequences based on combined min/max values
  min_val <- min(c(var_orig_hfa, var_syn_hfa), na.rm = TRUE)
  max_val <- max(c(var_orig_hfa, var_syn_hfa), na.rm = TRUE)
  cuts <- seq(min_val, max_val, length.out = bins_num)
  
  # Discretize nutrient variable in both data frames
  orig_cut <- cut(var_orig_hfa, breaks = cuts,
                  include.lowest = TRUE, ordered_result = TRUE)
  syn_cut <- cut(var_syn_hfa, breaks = cuts,
                 include.lowest = TRUE, ordered_result = TRUE)
  
  # Joint relative frequencies (orig vs syn)
  tab_orig <- prop.table(table(orig_cut, df_original_hfa[["hfa"]]))
  tab_syn <- prop.table(table(syn_cut, df_synthetic[["hfa"]]))
  
  # Calculate Jensen-Shannon Divergence
  mat_jsd1 <- rbind(tab_orig, tab_syn)
  jsd_value_hfa <- mean(
    philentropy::distance(mat_jsd1, method = "jensen-shannon", 
                          mute.message = TRUE),
    na.rm = TRUE
  )
  
  # Prepare data for plotting
  df_plot_orig_hfa <- dplyr::transmute(
    df_original_hfa,
    hfa = .data$hfa,
    fe_maiz = .data[[var_name]],
    origin = "Original"
  )
  df_plot_syn_hfa <- dplyr::transmute(
    df_synthetic,
    hfa = .data$hfa,
    fe_maiz = .data[[var_name]],
    origin = "Synthetic"
  )
  df_plot_hfa <- dplyr::bind_rows(df_plot_orig_hfa, df_plot_syn_hfa)
  
  # Generate plot (g1)
  g1 <- ggplot2::ggplot(df_plot_hfa, 
                        ggplot2::aes(y = .data$hfa, x = .data$fe_maiz)) +
    ggdist::stat_slab(
      data = dplyr::filter(df_plot_hfa, .data$origin == "Original"),
      fill = "#1f78b4",
      side = "top",
      alpha = 0.6,
      scale = 0.5
    ) +
    ggdist::stat_slab(
      data = dplyr::filter(df_plot_hfa, .data$origin == "Synthetic"),
      fill = "#a6cee3",
      side = "bottom",
      alpha = 0.6,
      scale = 0.5
    ) +
    # Limit X-axis to 75th percentile (adjustable)
    ggplot2::scale_x_continuous(
      limits = c(
        0,
        stats::quantile(df_plot_hfa$fe_maiz, 0.75, na.rm = TRUE)
      )
    ) +
    ggplot2::labs(
      title = paste0(
        "Distribution Comparison: ",
        "<span style='color:#1f78b4;'><b>Original</b></span> vs ",
        "<span style='color:#a6cee3;'><b>Synthetic</b></span>"
      ),
      subtitle = paste0(
        "Jensen-Shannon Divergence: ",
        formatC(jsd_value_hfa, digits = 3, format = "g")
      ),
      x = var_name,
      y = "Stunting status"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(color = "azure4", size = 15),
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 11),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 11, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # ===========================================================================
  # B) COMPARISON BY DEPARTMENT
  # ===========================================================================
  
  # Calculate J-S divergence for joint distribution "var_name" ~ department
  
  # Extract variable values (work with copies)
  var_orig_dept <- df_original_dept[[var_name]]
  var_syn_dept <- df_synthetic[[var_name]]
  
  # Create cut sequence
  min_val2 <- min(c(var_orig_dept, var_syn_dept), na.rm = TRUE)
  max_val2 <- max(c(var_orig_dept, var_syn_dept), na.rm = TRUE)
  cuts2 <- seq(min_val2, max_val2, length.out = bins_num)
  
  # Discretize
  orig_cut2 <- cut(var_orig_dept, breaks = cuts2,
                   include.lowest = TRUE, ordered_result = TRUE)
  syn_cut2 <- cut(var_syn_dept, breaks = cuts2,
                  include.lowest = TRUE, ordered_result = TRUE)
  
  tab_orig2 <- prop.table(table(orig_cut2, df_original_dept[["departamento"]]))
  tab_syn2 <- prop.table(table(syn_cut2, df_synthetic[["departamento"]]))
  
  mat_jsd2 <- rbind(tab_orig2, tab_syn2)
  jsd_value_dept <- mean(
    philentropy::distance(mat_jsd2, method = "jensen-shannon", 
                          mute.message = TRUE),
    na.rm = TRUE
  )
  
  # Prepare data for plotting
  df_plot_orig_dept <- dplyr::transmute(
    df_original_dept,
    departamento = .data$departamento,
    fe_maiz = .data[[var_name]],
    origin = "Original"
  )
  df_plot_syn_dept <- dplyr::transmute(
    df_synthetic,
    departamento = .data$departamento,
    fe_maiz = .data[[var_name]],
    origin = "Synthetic"
  )
  df_plot_dept <- dplyr::bind_rows(df_plot_orig_dept, df_plot_syn_dept)
  
  # Generate plot (g2)
  g2 <- ggplot2::ggplot(df_plot_dept, 
                        ggplot2::aes(y = .data$departamento, x = .data$fe_maiz)) +
    ggdist::stat_slab(
      data = dplyr::filter(df_plot_dept, .data$origin == "Original"),
      fill = "#1f78b4",
      side = "top",
      alpha = 0.6,
      scale = 0.5
    ) +
    ggdist::stat_slab(
      data = dplyr::filter(df_plot_dept, .data$origin == "Synthetic"),
      fill = "#a6cee3",
      side = "bottom",
      alpha = 0.6,
      scale = 0.5
    ) +
    ggplot2::scale_x_continuous(
      limits = c(
        0,
        stats::quantile(df_plot_dept$fe_maiz, 0.75, na.rm = TRUE)
      )
    ) +
    ggplot2::labs(
      subtitle = paste0(
        "Jensen-Shannon Divergence: ",
        formatC(jsd_value_dept, digits = 3, format = "g")
      ),
      x = paste0(var_name, " (mg/day)"),
      y = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(color = "azure4", size = 11),
      axis.line = ggplot2::element_line(color = "azure4"),
      axis.title = ggplot2::element_text(size = 11, colour = "azure4", hjust = 0),
      axis.text = ggplot2::element_text(size = 8, colour = "azure4")
    )
  
  # ===========================================================================
  # C) COMBINE PLOTS
  # ===========================================================================
  
  plot_grid <- ggpubr::ggarrange(
    g1, g2,
    ncol = 2, nrow = 1,
    widths = c(1, 1.5)
  )
  
  return(plot_grid)
}


# ==============================================================================
# 5. SURVEY DATA PROCESSING
# ==============================================================================

# ==============================================================================
# Adjust Nutrient Values Based on Predicted Stunting Status
# ==============================================================================
# Applies fine-tuning adjustment to synthetic nutrient values based on
# predicted stunting status and probability. Uses stratified multipliers to
# slightly increase nutrient values for children predicted to have stunting,
# proportional to prediction confidence.
#
# Arguments:
#   nutrient_value - Numeric vector with baseline nutrient values
#   hfa_predicted - Character/factor vector with stunting predictions ("Si"/"No")
#   hfa_probability - Numeric vector with prediction probabilities (0-1)
#
# Returns:
#   Numeric vector with adjusted nutrient values
#
# Details:
#   Adjustment multipliers for predicted stunting ("Si"):
#     - Probability 0.53-0.57: multiply by 1.01 (+1%)
#     - Probability 0.57-0.63: multiply by 1.02 (+2%)
#     - Probability 0.63-0.67: multiply by 1.03 (+3%)
#     - Probability 0.67-0.73: multiply by 1.04 (+4%)
#     - Probability 0.73-0.77: multiply by 1.05 (+5%)
#     - All other cases: no adjustment (multiply by 1.0)
#
# Example:
#   adjusted_zinc <- adjust_nutrient_by_hfa(
#     zinc_values,
#     hfa_predicted = c("Si", "No", "Si"),
#     hfa_probability = c(0.65, 0.40, 0.75)
#   )
# ==============================================================================
adjust_nutrient_by_hfa <- function(nutrient_value, hfa_predicted, hfa_probability) {
  
  # Check required packages are installed
  required_packages <- c("dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # Calculate adjustment multiplier based on stunting status and probability
  multiplier <- dplyr::case_when(
    hfa_predicted == "Si" & hfa_probability > 0.53 & hfa_probability <= 0.57 ~ 1.01,
    hfa_predicted == "Si" & hfa_probability > 0.57 & hfa_probability <= 0.63 ~ 1.02,
    hfa_predicted == "Si" & hfa_probability > 0.63 & hfa_probability <= 0.67 ~ 1.03,
    hfa_predicted == "Si" & hfa_probability > 0.67 & hfa_probability <= 0.73 ~ 1.04,
    hfa_predicted == "Si" & hfa_probability > 0.73 & hfa_probability <= 0.77 ~ 1.05,
    TRUE ~ 1
  )
  
  # Apply multiplier to nutrient values
  nutrient_value * multiplier
}

# ==============================================================================
# 6. DATA MANAGEMENT & UTILITIES
# ==============================================================================

# ==============================================================================
# Apply Controlled Jitter to Numeric Vector
# ==============================================================================
# Applies controlled random perturbation to a numeric vector for generating
# smooth transitions in synthetic population data. Jitter magnitude is
# proportional to a reference standard deviation, with hard bounds to prevent
# biologically implausible values.
#
# Arguments:
#   x - Numeric vector to jitter
#   sd_reference - Standard deviation from original sample (not bootstrapped)
#                  used as reference for jitter magnitude
#   intensity - Jitter magnitude as proportion of sd_reference (default 0.05)
#   lower_bound - Minimum allowable value after jitter (default -Inf)
#   upper_bound - Maximum allowable value after jitter (default Inf)
#   seed - Random seed for reproducibility (default NULL)
#
# Returns:
#   Numeric vector with jittered values, same length as input
#
# Details:
#   - Jitter is drawn from N(0, sd_reference * intensity)
#   - Values are clamped to [lower_bound, upper_bound] after jitter
#   - NA values are preserved (not jittered)
#   - If sd_reference is 0 or all values are NA, returns input unchanged
#
# Example:
#   # Apply 5% jitter to iron intake with non-negative constraint
#   fe_jittered <- apply_controlled_jitter(
#     x = df$fe_mg_maiz,
#     sd_reference = 2.5,
#     intensity = 0.05,
#     lower_bound = 0
#   )
#
#   # Apply 3% jitter to z-scores with WHO bounds
#   zlen_jittered <- apply_controlled_jitter(
#     x = df$zlen,
#     sd_reference = 1.2,
#     intensity = 0.03,
#     lower_bound = -6,
#     upper_bound = 6
#   )
# ==============================================================================
apply_controlled_jitter <- function(x,
                                    sd_reference,
                                    intensity = 0.05,
                                    lower_bound = -Inf,
                                    upper_bound = Inf
                                    ) {
  
  set.seed(789)
  
  
  # Validate inputs
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector.")
  }
  
  if (!is.numeric(sd_reference) || length(sd_reference) != 1) {
    stop("'sd_reference' must be a single numeric value.")
  }
  
  if (!is.numeric(intensity) || length(intensity) != 1 || intensity < 0) {
    stop("'intensity' must be a single non-negative numeric value.")
  }
  
  # Return unchanged if all NA or zero SD
  if (all(is.na(x)) || sd_reference == 0 || is.na(sd_reference)) {
    return(x)
  }
  
  # Generate noise
  noise <- stats::rnorm(length(x), mean = 0, sd = sd_reference * intensity)
  
  # Apply noise (preserving NAs)
  x_jittered <- x + noise
  
  # Apply bounds
  x_jittered <- pmax(x_jittered, lower_bound)
  x_jittered <- pmin(x_jittered, upper_bound)
  
  return(x_jittered)
}


# ==============================================================================
# Get Variable Bounds by Name Pattern
# ==============================================================================
# Retrieves appropriate biological or logical bounds for a variable based on
# pattern matching against its name. Used in conjunction with apply_controlled_jitter
# to ensure jittered values remain plausible.
#
# Arguments:
#   var_name - Character string with variable name to match
#   bounds_list - Named list where names are regex patterns and values are
#                 numeric vectors c(lower, upper)
#   default_bounds - Default bounds if no pattern matches (default c(-Inf, Inf))
#
# Returns:
#   Numeric vector of length 2: c(lower_bound, upper_bound)
#
# Details:
#   - Patterns are checked in order; first match wins
#   - Uses stringr::str_detect for pattern matching
#   - If no pattern matches, returns default_bounds
#
# Example:
#   bounds_config <- list(
#     "^zlen$" = c(-6, 6),
#     "^edad_mes" = c(6, 59),
#     "^per_" = c(0, 100),
#     "^fe_|^zn_|^prot_" = c(0, Inf)
#   )
#
#   get_variable_bounds("zlen", bounds_config)
#   # Returns: c(-6, 6)
#
#   get_variable_bounds("fe_mg_maiz", bounds_config)
#   # Returns: c(0, Inf)
#
#   get_variable_bounds("unknown_var", bounds_config)
#   # Returns: c(-Inf, Inf)
# ==============================================================================
get_variable_bounds <- function(var_name,
                                bounds_list,
                                default_bounds = c(-Inf, Inf)) {
  
  # Check required packages
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required but not installed.")
  }
  
  # Validate inputs
  if (!is.character(var_name) || length(var_name) != 1) {
    stop("'var_name' must be a single character string.")
  }
  
  if (!is.list(bounds_list)) {
    stop("'bounds_list' must be a named list.")
  }
  
  # Check each pattern in order
  for (pattern in names(bounds_list)) {
    if (stringr::str_detect(var_name, pattern)) {
      return(bounds_list[[pattern]])
    }
  }
  
  # No match found
  return(default_bounds)
}


# ==============================================================================
# Apply Jitter to Data Frame Column Set
# ==============================================================================
# Applies controlled jitter to a specified set of columns in a data frame,
# using reference SDs from an original (non-bootstrapped) data frame.
# Returns a data frame with only the jittered columns plus an ID column.
#
# Arguments:
#   df - Data frame containing columns to jitter
#   df_reference - Original data frame for calculating reference SDs
#   vars_to_jitter - Character vector of column names to jitter
#   id_col - Name of ID column to retain (default "synthetic_id")
#   intensity - Jitter magnitude as proportion of SD (default 0.05)
#   bounds_list - Named list of bounds by variable pattern (default NULL)
#   default_bounds - Default bounds if no pattern matches (default c(-Inf, Inf))
#   seed - Random seed for reproducibility (default NULL)
#
# Returns:
#   Data frame with id_col and jittered versions of vars_to_jitter
#
# Details:
#   - Only numeric columns in vars_to_jitter are processed
#   - Non-numeric columns are skipped with a warning
#   - Reference SDs are calculated from df_reference, not df
#   - Columns not in df are skipped with a warning
#
# Example:
#   df_jittered_nutritional <- apply_jitter_to_columns(
#     df = df_synthetic,
#     df_reference = df_original,
#     vars_to_jitter = c("fe_mg_maiz", "zn_mg_maiz", "per_vegetales"),
#     id_col = "synthetic_id",
#     intensity = 0.05,
#     bounds_list = list("^fe_|^zn_" = c(0, Inf), "^per_" = c(0, 100)),
#     seed = 2025
#   )
# ==============================================================================
apply_jitter_to_columns <- function(df,
                                    df_reference,
                                    vars_to_jitter,
                                    id_col = "synthetic_id",
                                    intensity = 0.05,
                                    bounds_list = NULL,
                                    default_bounds = c(-Inf, Inf)
                                    ) {
  
  set.seed(789)
  
  # Validate ID column exists
  if (!id_col %in% names(df)) {
    stop(paste0("ID column '", id_col, "' not found in data frame."))
  }
  
  # Initialize result with ID column
  result <- df[, id_col, drop = FALSE]
  
  # Track skipped variables
  skipped_missing <- character(0)
  skipped_nonnumeric <- character(0)
  
  # Process each variable
  for (var in vars_to_jitter) {
    
    # Check if variable exists
    if (!var %in% names(df)) {
      skipped_missing <- c(skipped_missing, var)
      next
    }
    
    # Check if numeric
    if (!is.numeric(df[[var]])) {
      skipped_nonnumeric <- c(skipped_nonnumeric, var)
      next
    }
    
    # Calculate reference SD
    sd_ref <- stats::sd(df_reference[[var]], na.rm = TRUE)
    
    # Get bounds
    if (!is.null(bounds_list)) {
      bounds <- get_variable_bounds(var, bounds_list, default_bounds)
    } else {
      bounds <- default_bounds
    }
    
    # Apply jitter
    result[[var]] <- apply_controlled_jitter(
      x = df[[var]],
      sd_reference = sd_ref,
      intensity = intensity,
      lower_bound = bounds[1],
      upper_bound = bounds[2]
    )
  }
  
  # Report skipped variables
  if (length(skipped_missing) > 0) {
    warning(paste0("Variables not found in data frame: ", 
                   paste(skipped_missing, collapse = ", ")))
  }
  
  if (length(skipped_nonnumeric) > 0) {
    warning(paste0("Non-numeric variables skipped: ", 
                   paste(skipped_nonnumeric, collapse = ", ")))
  }
  
  return(result)
}

# ==============================================================================
# Calibrate Survey Weights by Group
# ==============================================================================
# Calibrates survey weights within groups (e.g., departments) to match target
# totals using survey::calibrate() with linear calibration function.
#
# This function is designed for post-stratification calibration where we want
# weighted aggregates to match official statistics (e.g., ENSMI stunting totals,
# MAGA production totals) while preserving individual-level variation.
#
# Arguments:
#   df - Data frame containing the data to calibrate
#   group_var - Character string naming the grouping variable (e.g., "departamento")
#   id_var - Character string naming the unique identifier variable
#   weight_var - Character string naming the initial weight variable
#   calibration_vars - Character vector of variable names to calibrate on
#   df_targets - Data frame with targets, must contain:
#                - Column matching group_var name
#                - Columns named "target_{var}" for each var in calibration_vars
#                  (targets must be TOTALS, not rates)
#   bounds - Numeric vector of length 2 with lower and upper bounds for weight
#            adjustment ratios (default: c(0.0001, 100000))
#   maxit - Maximum number of iterations for calibration algorithm (default: 500)
#   preserve_total - Logical, whether to preserve weighted total per group via
#                    intercept constraint (default: TRUE)
#
# Returns:
#   Data frame with original data plus new column "weight_calibrated"
#
# Details:
#   - Uses the linear calibration function
#   - Processes each group independently
#   - If calibration fails for a group, returns original weights with warning
#   - Targets must be expressed as TOTALS (for rates: total = N × rate)
#
# Example:
#   # Calibrate stunting and chispitas totals by department
#   df_targets <- df_official |>
#     mutate(
#       target_hfa_predicted = N_weighted * stunting_rate,
#       target_receives_chispitas = N_weighted * chispitas_coverage
#     )
#
#   df_cal <- calibrate_weights_by_group(
#     df = df_synthetic,
#     group_var = "departamento",
#     id_var = "synthetic_id",
#     weight_var = "weight_initial",
#     calibration_vars = c("hfa_predicted", "receives_chispitas"),
#     df_targets = df_targets
#   )
#
#   # Calibrate production and land totals by department (MAGA style)
#   df_cal <- calibrate_weights_by_group(
#     df = df_farmers,
#     group_var = "departamento",
#     id_var = "no_hogar",
#     weight_var = "factor_ponderado",
#     calibration_vars = c("total_production", "land_wtcorn"),
#     df_targets = df_maga_targets
#   )
# ==============================================================================

calibrate_weights_by_group <- function(df,
                                       group_var,
                                       id_var,
                                       weight_var,
                                       calibration_vars,
                                       df_targets,
                                       bounds = c(0.0001, 100000),
                                       maxit = 500,
                                       preserve_total = TRUE) {
  
  # --- Validate inputs ---
  if (!group_var %in% names(df)) {
    stop(paste("group_var", group_var, "not found in df"))
  }
  if (!id_var %in% names(df)) {
    stop(paste("id_var", id_var, "not found in df"))
  }
  if (!weight_var %in% names(df)) {
    stop(paste("weight_var", weight_var, "not found in df"))
  }
  if (!all(calibration_vars %in% names(df))) {
    missing <- setdiff(calibration_vars, names(df))
    stop(paste("calibration_vars not found in df:", paste(missing, collapse = ", ")))
  }
  
  # Check target columns exist
  target_cols <- paste0("target_", calibration_vars)
  if (!all(target_cols %in% names(df_targets))) {
    missing <- setdiff(target_cols, names(df_targets))
    stop(paste("Target columns not found in df_targets:", paste(missing, collapse = ", ")))
  }
  
  # --- Get unique groups ---
  groups <- unique(df[[group_var]])
  
  # --- Process each group ---
  results <- purrr::map_dfr(groups, function(grp) {
    
    # Filter to group
    df_grp <- df[df[[group_var]] == grp, ]
    
    # Get targets for this group
    targets_grp <- df_targets[df_targets[[group_var]] == grp, ]
    
    if (nrow(targets_grp) == 0) {
      warning(paste("No targets found for group:", grp, "- returning original weights"))
      df_grp$weight_calibrated <- df_grp[[weight_var]]
      return(df_grp)
    }
    
    # Create survey design
    design_formula <- stats::as.formula(paste("~", id_var))
    weight_vec <- df_grp[[weight_var]]
    
    sd_grp <- survey::svydesign(
      ids = design_formula,
      strata = NULL,
      weights = weight_vec,
      data = df_grp
    )
    
    # Calculate weighted total for group (for intercept constraint)
    N_weighted <- sum(weight_vec)
    
    # Build population totals vector
    pop_totals <- c()
    
    if (preserve_total) {
      pop_totals["(Intercept)"] <- N_weighted
    }
    
    for (var in calibration_vars) {
      target_col <- paste0("target_", var)
      pop_totals[var] <- targets_grp[[target_col]]
    }
    
    # Build calibration formula
    cal_formula <- stats::as.formula(
      paste("~", paste(calibration_vars, collapse = " + "))
    )
    
    # Attempt calibration with linear function
    sd_calibrated <- tryCatch({
      suppressWarnings(
        survey::calibrate(
          design = sd_grp,
          formula = cal_formula,
          population = pop_totals,
          calfun = "linear",
          bounds = bounds,
          maxit = maxit
        )
      )
    }, error = function(e) {
      warning(paste("Calibration failed for group", grp, ":", e$message,
                    "- returning original weights"))
      return(NULL)
    })
    
    # Extract calibrated weights using weights() function
    if (is.null(sd_calibrated)) {
      df_grp$weight_calibrated <- df_grp[[weight_var]]
    } else {
      df_grp$weight_calibrated <- as.numeric(weights(sd_calibrated))
    }
    
    return(df_grp)
  })
  
  return(results)
}

# ==============================================================================
# Impute Missing Values with Random Forest (Parallel)
# ==============================================================================
# Performs multivariate missing value imputation using Random Forest, with
# native multi-threading via the ranger engine. Returns the imputed data
# alongside aggregated out-of-bag error metrics for continuous and categorical
# variables.
#
# Arguments:
#   data - Data frame with missing values to impute. Mixed continuous and
#          categorical variables are supported. Character columns should be
#          converted to factors beforehand.
#   maxiter - Integer. Maximum number of iterations for the imputation
#             procedure (default: 10).
#   ntree - Integer. Number of trees per Random Forest (default: 100).
#           Passed internally as num.trees to ranger.
#   num_threads - Integer. Number of threads for parallel tree construction
#                 (default: parallel::detectCores() - 2). Set to 1 to enforce
#                 bit-level reproducibility at the cost of performance.
#   verbose - Logical. Whether to print iteration progress (default: FALSE).
#
# Returns:
#   List with the imputed data, aggregated out-of-bag errors, and the count of
#   variables of each type that required imputation:
#     - ximp: Data frame with imputed values, same dimensions as input
#     - OOBerror: Named numeric vector with two elements:
#         * OOBE: Out-of-bag error across the continuous variables imputed,
#                 defined as the square root of the variance-weighted mean of
#                 the per-variable unexplained variance,
#                   OOBE = sqrt( sum_j(MSE_j) / sum_j(Var_j) ),
#                 where Var_j is the variance of the observed values of column
#                 j and MSE_j its out-of-bag mean squared error. The metric is
#                 bounded in [0, 1] and invariant to the scale and location of
#                 each column; 0.5 corresponds to a variance-weighted mean
#                 out-of-bag R-squared of 0.75. Returns NA_real_ if no
#                 continuous variable required imputation.
#         * PFC:  Proportion of Falsely Classified entries, averaged across the
#                 categorical variables imputed. Returns NA_real_ if no
#                 categorical variable required imputation.
#     - n_continuous:  Integer count of continuous variables imputed. 0
#                      identifies the OOBE = NA case as "none required
#                      imputation".
#     - n_categorical: Integer count of categorical variables imputed. 0
#                      identifies the PFC = NA case likewise.
#
# Details:
#   ranger fits categorical and logical responses by classification and numeric
#   responses by regression, reporting per-variable out-of-bag errors on those
#   two scales. This function aggregates them into the two summary metrics
#   consumed by the diagnostic tables of the preparation scripts.
#
# Example:
#   set.seed(789)
#   imputed_result <- impute_rf_parallel(
#     data = predictors_to_impute,
#     maxiter = 10,
#     ntree = 100
#   )
#   df_imputed <- imputed_result$ximp
#   oobe_value <- imputed_result$OOBerror["OOBE"]
#   pfc_value <- imputed_result$OOBerror["PFC"]
# ==============================================================================
impute_rf_parallel <- function(data,
                               maxiter = 10,
                               ntree = 100,
                               num_threads = parallel::detectCores() - 2,
                               verbose = FALSE) {
  
  # Check required packages are installed
  required_packages <- c("missRanger", "dplyr", "tibble")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data frame.")
  }
  
  if (!is.numeric(maxiter) || length(maxiter) != 1 || maxiter < 1) {
    stop("'maxiter' must be a single positive integer.")
  }
  
  if (!is.numeric(ntree) || length(ntree) != 1 || ntree < 1) {
    stop("'ntree' must be a single positive integer.")
  }
  
  if (!is.numeric(num_threads) || length(num_threads) != 1 || num_threads < 1) {
    stop("'num_threads' must be a single positive integer.")
  }
  
  # --- Identify variable types ------------------------------------------------
  # Types follow the response ranger fits: numeric regression, the rest class.
  is_continuous <- vapply(data, is.numeric, logical(1))
  is_categorical <- vapply(
    data,
    function(x) is.factor(x) || is.character(x) || is.logical(x),
    logical(1)
  )

  # --- Run imputation ---------------------------------------------------------
  imputation_raw <- missRanger::missRanger(
    data = data,
    maxiter = maxiter,
    num.trees = ntree,
    num.threads = num_threads,
    verbose = as.integer(verbose),
    returnOOB = TRUE
  )
  
  # --- Extract per-variable OOB errors ----------------------------------------
  # Only variables that required imputation appear in the attribute.
  oob_per_var <- attr(imputation_raw, "oob")

  # --- Subset per-variable errors by type -------------------------------------
  continuous_vars    <- names(data)[is_continuous]
  categorical_vars   <- names(data)[is_categorical]
  continuous_errors  <- oob_per_var[names(oob_per_var) %in% continuous_vars]
  categorical_errors <- oob_per_var[names(oob_per_var) %in% categorical_vars]

  n_continuous  <- length(continuous_errors)
  n_categorical <- length(categorical_errors)

  # --- Aggregate out-of-bag errors --------------------------------------------
  # Weight the per-variable 1 - R^2 by Var_j to get sqrt(sum(MSE_j)/sum(Var_j))
  if (n_continuous > 0) {
    continuous_variances <- vapply(
      names(continuous_errors),
      function(v) stats::var(data[[v]], na.rm = TRUE),
      numeric(1)
    )
    oobe_value <- sqrt(
      stats::weighted.mean(continuous_errors, w = continuous_variances,
                           na.rm = TRUE)
    )
  } else {
    oobe_value <- NA_real_
  }

  # Classification error averaged across the categorical variables imputed.
  if (n_categorical > 0) {
    pfc_value <- mean(categorical_errors, na.rm = TRUE)
  } else {
    pfc_value <- NA_real_
  }

  # --- Assemble output list ---------------------------------------------------
  # Counts let diagnostic tables tell a missing metric from an absent type
  result <- list(
    ximp = tibble::as_tibble(imputation_raw),
    OOBerror = c(OOBE = oobe_value, PFC = pfc_value),
    n_continuous  = n_continuous,
    n_categorical = n_categorical
  )

  return(result)
}

# ==============================================================================
# Build Stunting Model Formula
# ==============================================================================
# Constructs a model formula for stunting outcomes (binary stunting and
# continuous HAZ) fully data-driven from the current state of the pipeline:
#
#   1. Start from the Boruta-selected variables (Confirmed + Tentative).
#   2. Subtract the correlation filter set (variables with |r| > threshold
#      against a reference variable, typically nutritional_index).
#   3. Add the mandatory predictors (nutritional_index, any other
#      explicit terms the modeler decides to always include).
#   4. For each numeric predictor, determine its polynomial degree (1, 2
#      or 3) via determine_poly_degree(). Categorical predictors are
#      always linear.
#   5. Assemble the final formula as outcome ~ sum of terms.
#
# Polynomial degree determination is parallelized via mirai. The caller
# can control pool size or let the function size it automatically.
#
# Arguments:
#   data:                    training data.frame
#   outcome:                 character, outcome column name
#   boruta_vars:             character vector of Boruta-selected predictors
#   mandatory_vars:          character vector of predictors always included
#                            (e.g. c("nutritional_index")). These are NOT
#                            subject to the correlation filter.
#   correlation_excluded:    character vector of predictors to drop because
#                            of collinearity with the correlation reference.
#                            Pass the variables the A5 section of the
#                            calling script has marked as Exclude = Yes.
#   weights:                 numeric vector of survey weights for the GAM
#                            fits; NULL for unweighted.
#   family:                  gam family used in determine_poly_degree().
#                            Use binomial("logit") for the binary stunting
#                            outcome, gaussian() for HAZ.
#   k:                       basis dimension for the univariate GAMs.
#                            Passed to determine_poly_degree().
#   edf_cuts:                numeric length-2. Passed to
#                            determine_poly_degree().
#   linear_only:             character vector of predictors to force as
#                            linear (skip GAM check). Use for PCs or
#                            dummy-encoded variables where polynomials
#                            make no semantic sense.
#   n_daemons:               integer, size of the mirai pool. If NULL,
#                            defaults to min(n_numeric_vars,
#                            max(1, physical_cores - 4)).
#
# Returns:
#   A named list:
#     formula:       the constructed formula object
#     audit:         tibble with one row per candidate variable, columns:
#                      variable, source, action, degree, edf, status
#                    source    ∈ {"mandatory", "boruta"}
#                    action    ∈ {"kept", "dropped_correlation",
#                                 "forced_linear", "gam_inferred"}
#                    degree    final polynomial degree (1/2/3) or NA
#                    edf       edf from GAM or NA
#                    status    "ok" / "fit_failed" / "forced" / etc.
#     n_terms:       integer, number of terms in the final formula
#
# Example:
#   res <- build_stunting_formula(
#     data = df_model,
#     outcome = "zlen",
#     boruta_vars = readRDS(here(
#       "01_data", "02_processed", "models",
#       "04_05_boruta_selected_variables.rds"
#     )),
#     mandatory_vars = c("nutritional_index"),
#     correlation_excluded = c("grado_estudios_hogar", "pc_wealth_1"),
#     weights = df_model$pesohogar,
#     family = gaussian(),
#     linear_only = grep("^pc_", names(df_model), value = TRUE)
#   )
#   svyglm(res$formula, design = survey_design)
# ==============================================================================

build_stunting_formula <- function(data,
                                   outcome,
                                   boruta_vars,
                                   mandatory_vars,
                                   correlation_excluded = character(0),
                                   weights = NULL,
                                   family = gaussian(),
                                   k = 10,
                                   edf_cuts = c(1.3, 2.2),
                                   linear_only = character(0),
                                   n_daemons = NULL) {
  
  # --- Input validation ----------------------------------------------------
  if (!is.data.frame(data)) stop("'data' must be a data.frame")
  if (!outcome %in% names(data)) {
    stop("Outcome '", outcome, "' not found in data")
  }
  
  # --- Assemble candidate variable set -------------------------------------
  # Priority: mandatory always kept (bypass correlation filter); boruta
  # loses any variable flagged as correlation_excluded.
  mandatory_vars <- unique(as.character(mandatory_vars))
  boruta_vars    <- unique(as.character(boruta_vars))
  
  boruta_kept  <- setdiff(boruta_vars, correlation_excluded)
  candidates   <- unique(c(mandatory_vars, boruta_kept))
  
  # Only variables that actually exist in the data survive
  existing <- intersect(candidates, names(data))
  missing_in_data <- setdiff(candidates, existing)
  
  # --- Build initial audit trail -------------------------------------------
  audit_init <- tibble::tibble(
    variable = c(boruta_vars, mandatory_vars),
    source   = c(rep("boruta",    length(boruta_vars)),
                 rep("mandatory", length(mandatory_vars)))
  ) |>
    dplyr::distinct(variable, .keep_all = TRUE) |>
    dplyr::mutate(
      action = dplyr::case_when(
        variable %in% correlation_excluded & source == "boruta" ~
          "dropped_correlation",
        variable %in% missing_in_data ~ "dropped_not_in_data",
        TRUE ~ "pending"
      )
    )
  
  # --- Split candidates by type --------------------------------------------
  is_numeric_col <- vapply(
    existing,
    function(v) is.numeric(data[[v]]),
    logical(1)
  )
  numeric_vars     <- existing[is_numeric_col]
  categorical_vars <- existing[!is_numeric_col]
  
  # Variables forced to linear (PCs, etc.) bypass the GAM step
  numeric_gam  <- setdiff(numeric_vars, linear_only)
  numeric_forced <- intersect(numeric_vars, linear_only)
  
  # --- Parallel GAM univariate fits ---------------------------------------
  # The 30-60 candidate GAM fits are independent. Parallelize via a local
  # mirai pool, sized conservatively.
  physical_cores <- parallel::detectCores(logical = FALSE)
  if (is.null(n_daemons)) {
    n_daemons <- min(
      max(length(numeric_gam), 1L),
      max(1L, physical_cores - 4L)
    )
  }
  
  gam_results <- list()
  if (length(numeric_gam) > 0) {
    
    mirai_profile <- "build_formula_gam"
    mirai::daemons(n_daemons, .compute = mirai_profile)
    on.exit(mirai::daemons(0, .compute = mirai_profile), add = TRUE)
    
    mirai::everywhere(
      {
        if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
          RhpcBLASctl::blas_set_num_threads(1)
          RhpcBLASctl::omp_set_num_threads(1)
        }
      },
      .compute = mirai_profile
    )
    
    gam_results <- mirai::mirai_map(
      .x    = numeric_gam,
      .f    = function(var_name,
                       data_local,
                       outcome_local,
                       weights_local,
                       family_local,
                       k_local,
                       edf_cuts_local,
                       fun_determine) {
        fun_determine(
          data      = data_local,
          predictor = var_name,
          outcome   = outcome_local,
          weights   = weights_local,
          family    = family_local,
          k         = k_local,
          edf_cuts  = edf_cuts_local
        )
      },
      .args = list(
        data_local     = data,
        outcome_local  = outcome,
        weights_local  = weights,
        family_local   = family,
        k_local        = k,
        edf_cuts_local = edf_cuts,
        fun_determine  = determine_poly_degree
      ),
      .compute = mirai_profile
    )[]
    
    names(gam_results) <- numeric_gam
  }
  
  # --- Build per-variable degree lookup -----------------------------------
  degree_map <- list()
  for (v in numeric_gam) {
    degree_map[[v]] <- gam_results[[v]]
  }
  for (v in numeric_forced) {
    degree_map[[v]] <- list(
      degree = 1L, edf = NA_real_,
      status = "forced", reason = "linear_only override"
    )
  }
  
  # --- Compose final audit tibble -----------------------------------------
  # Extract degree, edf and status per variable into named lookups
  # (avoids multi-line if/else inside purrr::map_* lambdas).
  degree_lookup <- vapply(
    names(degree_map),
    function(v) as.integer(degree_map[[v]]$degree),
    integer(1)
  )
  edf_lookup <- vapply(
    names(degree_map),
    function(v) {
      val <- degree_map[[v]]$edf
      if (is.null(val)) NA_real_ else as.numeric(val)
    },
    numeric(1)
  )
  status_lookup <- vapply(
    names(degree_map),
    function(v) {
      val <- degree_map[[v]]$status
      if (is.null(val)) NA_character_ else as.character(val)
    },
    character(1)
  )
  
  audit_final <- audit_init |>
    dplyr::mutate(
      degree = dplyr::case_when(
        variable %in% categorical_vars     ~ NA_integer_,
        variable %in% names(degree_lookup) ~ degree_lookup[variable],
        TRUE                               ~ NA_integer_
      ),
      edf = dplyr::case_when(
        variable %in% names(edf_lookup) ~ edf_lookup[variable],
        TRUE                            ~ NA_real_
      ),
      status = dplyr::case_when(
        variable %in% categorical_vars     ~ "categorical",
        variable %in% names(status_lookup) ~ status_lookup[variable],
        TRUE                               ~ "excluded"
      ),
      action = dplyr::case_when(
        action %in% c("dropped_correlation", "dropped_not_in_data") ~ action,
        variable %in% categorical_vars                              ~ "kept_categorical",
        variable %in% numeric_forced                                ~ "forced_linear",
        variable %in% numeric_gam                                   ~ "gam_inferred",
        TRUE                                                        ~ action
      )
    )
  
  # --- Compose formula string ---------------------------------------------
  # Categorical terms go as-is; numeric terms get poly() wrapper if degree > 1.
  term_for_variable <- function(v) {
    if (v %in% categorical_vars) return(v)
    d <- degree_map[[v]]$degree
    if (is.na(d) || d <= 1L) return(v)
    paste0("poly(", v, ", ", d, ", raw = TRUE)")
  }
  
  # Preserve stable ordering: mandatory first, then boruta (alphabetical)
  ordered_terms <- c(
    intersect(mandatory_vars, existing),
    sort(setdiff(intersect(boruta_kept, existing), mandatory_vars))
  )
  
  rhs <- vapply(ordered_terms, term_for_variable, character(1))
  formula_text <- paste(outcome, "~", paste(rhs, collapse = " +\n  "))
  final_formula <- as.formula(formula_text)
  
  list(
    formula = final_formula,
    audit   = audit_final,
    n_terms = length(rhs)
  )
}

# ==============================================================================
# Load SIVESNU Data Dictionary
# ==============================================================================
# Loads the operational data dictionary CSV and parses domain bounds and factor
# levels into structured R objects. The dictionary defines the universe of valid
# values for every variable in each SIVESNU 2018 source table (nino, mujer,
# hogar, miembros).
#
# Arguments:
#   dictionary_path - Character path to the dictionary CSV file
#                     (typically `02_code/_config/sivesnu_data_dictionary.csv`)
#
# Returns:
#   Tibble with the dictionary contents and parsed columns:
#     - name: variable name (snake_case as it appears in the data)
#     - name_original: original name from the SIVESNU questionnaire
#     - source_table: nino | mujer | hogar | miembros
#     - section: section in the original SIVESNU dictionary
#     - label_es: Spanish label
#     - type: factor | binary | numeric_continuous | numeric_integer |
#             id | date | text
#     - domain_min: numeric lower bound (NA for non-numeric)
#     - domain_max: numeric upper bound (NA for non-numeric)
#     - domain_levels: character vector of valid values
#                      (empty for non-categorical types)
#     - unit: unit of measurement
#     - notes: free-text notes
#
# Details:
#   - The same variable name may appear in multiple source tables with
#     different valid universes (e.g. fies1 in hogar uses 1/2/98 codes
#     while a derived fies1 in the modeling pipeline uses 0/1). The
#     (source_table, name) pair is the unique key.
#   - domain_levels in the CSV are stored as semicolon-separated strings;
#     this function parses them into list-columns of character vectors.
#   - All character columns are read as UTF-8.
#
# Example:
#   data_dict <- load_data_dictionary(
#     here::here("02_code", "_config", "sivesnu_data_dictionary.csv")
#   )
# ==============================================================================
load_data_dictionary <- function(dictionary_path) {
  
  # Check required packages are installed
  required_packages <- c("readr", "dplyr", "stringr", "tibble")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  if (!file.exists(dictionary_path)) {
    stop(paste0("Dictionary file not found at: ", dictionary_path))
  }
  
  data_dict <- readr::read_csv(
    dictionary_path,
    col_types = readr::cols(
      name           = readr::col_character(),
      name_original  = readr::col_character(),
      source_table   = readr::col_character(),
      section        = readr::col_character(),
      label_es       = readr::col_character(),
      type           = readr::col_character(),
      domain_min     = readr::col_character(),
      domain_max     = readr::col_character(),
      domain_levels  = readr::col_character(),
      unit           = readr::col_character(),
      notes          = readr::col_character()
    ),
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )
  
  data_dict |>
    dplyr::mutate(
      domain_min = suppressWarnings(as.numeric(domain_min)),
      domain_max = suppressWarnings(as.numeric(domain_max)),
      domain_levels = stringr::str_split(
        dplyr::if_else(is.na(domain_levels), "", domain_levels),
        ";"
      ),
      domain_levels = lapply(domain_levels, function(x) {
        x <- stringr::str_trim(x)
        x[x != ""]
      })
    )
}


# ==============================================================================
# Audit Variables Against Data Dictionary
# ==============================================================================
# Audits a SIVESNU source table (nino, mujer, hogar, or miembros) against the
# operational data dictionary. Out-of-domain values are recoded to NA; empty
# strings are also normalized to NA before the comparison. The function returns
# the sanitized dataframe and a per-variable incidence summary.
#
# The audit applies a domain-only validation rule: every variable has a declared
# universe of valid values (factor levels for categorical, [min, max] for
# numeric); values outside that universe are recoded to NA regardless of whether
# they correspond to documented non-response codes or to artefacts of upstream
# transformations. After the domain rule is applied, each audited variable is
# coerced to the R class implied by the dictionary (factor for binary/factor,
# integer for numeric_integer, numeric for numeric_continuous), so that
# downstream stages (imputation, modelling) operate on semantically correct
# types. Variables of type id, date, and text are passed through unchanged.
#
# Arguments:
#   data - Single SIVESNU source table dataframe (after clean_names() and
#          zap_*() but before merge with other tables)
#   data_dictionary - Tibble produced by load_data_dictionary()
#   source_table - Character matching the table being audited
#                  (one of "nino", "mujer", "hogar", "miembros")
#
# Returns:
#   List with three elements:
#     - data: dataframe with out-of-domain values and empty strings recoded to
#         NA, and audited columns coerced to the R class declared in the
#         dictionary
#     - incidents_summary: tibble with one row per audited variable, columns:
#         source_table, variable, type, n_total, n_pre_na, n_recoded,
#         n_post_na, pct_recoded, audit_status
#     - source_table: the source_table argument echoed back for traceability
#
# Details:
#   - Audits variables whose type is binary, factor, numeric_continuous, or
#     numeric_integer. Variables of type id, date, or text are passed through
#   - Empty strings ("") in character columns are normalized to NA before the
#     comparison and counted as such in n_pre_na
#   - For binary and factor: values not present in domain_levels are recoded
#     to NA. The comparison handles three encodings: original character form,
#     leading-zero-stripped numeric form ("01" matches "1"), and original
#     numeric form for numeric-coded factors. After recoding, the column is
#     coerced to factor() with levels = domain_levels; values that matched via
#     the leading-zero-stripped form are remapped back to the literal level
#   - For numeric_continuous and numeric_integer: values strictly outside
#     [domain_min, domain_max] are recoded to NA. After recoding, the column
#     is coerced to integer() (numeric_integer) or numeric() (numeric_continuous)
#   - Variables in data but not in the dictionary subset for this source_table
#     are reported with audit_status = "not_in_dictionary" and passed through
#     unchanged
#
# Example:
#   data_dict <- load_data_dictionary(
#     here::here("02_code", "_config", "sivesnu_data_dictionary.csv")
#   )
#   audit_nino <- audit_variables_against_dictionary(
#     data = sivesnu_nino,
#     data_dictionary = data_dict,
#     source_table = "nino"
#   )
#   sivesnu_nino_audited <- audit_nino$data
# ==============================================================================
audit_variables_against_dictionary <- function(data,
                                               data_dictionary,
                                               source_table) {
  
  required_packages <- c("dplyr", "tibble", "stringr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  if (!is.data.frame(data_dictionary)) {
    stop("'data_dictionary' must be produced by load_data_dictionary().")
  }
  required_cols <- c("name", "source_table", "type", "domain_min",
                     "domain_max", "domain_levels")
  missing_cols <- setdiff(required_cols, names(data_dictionary))
  if (length(missing_cols) > 0) {
    stop(paste0("data_dictionary is missing required columns: ",
                paste(missing_cols, collapse = ", ")))
  }
  if (!source_table %in% c("nino", "mujer", "hogar", "miembros")) {
    stop("'source_table' must be one of: nino, mujer, hogar, miembros.")
  }
  
  # Capture parameter value to avoid name collision in dplyr::filter
  table_filter <- source_table
  
  # Subset dictionary to the table being audited
  dict_table <- data_dictionary |>
    dplyr::filter(source_table == table_filter)
  
  audited_types <- c("binary", "factor", "numeric_continuous", "numeric_integer")
  passthrough_types <- c("id", "date", "text")
  
  data_columns <- names(data)
  incidents <- vector("list", length(data_columns))
  data_audited <- data
  
  for (i in seq_along(data_columns)) {
    
    col_name <- data_columns[i]
    column_values <- data[[col_name]]
    
    # Normalize empty strings to NA in character columns
    if (is.character(column_values)) {
      empty_mask <- !is.na(column_values) & column_values == ""
      if (any(empty_mask)) {
        column_values[empty_mask] <- NA_character_
        data_audited[[col_name]] <- column_values
      }
    }
    
    n_total <- length(column_values)
    n_pre_na <- sum(is.na(column_values))
    
    # Lookup dictionary entry
    dict_entry <- dict_table |> dplyr::filter(name == col_name)
    
    if (nrow(dict_entry) == 0) {
      incidents[[i]] <- tibble::tibble(
        source_table = source_table,
        variable = col_name,
        type = NA_character_,
        n_total = n_total,
        n_pre_na = n_pre_na,
        n_recoded = 0L,
        n_post_na = n_pre_na,
        pct_recoded = 0,
        audit_status = "not_in_dictionary"
      )
      next
    }
    
    var_type <- dict_entry$type[[1]]
    
    if (var_type %in% passthrough_types) {
      incidents[[i]] <- tibble::tibble(
        source_table = source_table,
        variable = col_name,
        type = var_type,
        n_total = n_total,
        n_pre_na = n_pre_na,
        n_recoded = 0L,
        n_post_na = n_pre_na,
        pct_recoded = 0,
        audit_status = "passthrough"
      )
      next
    }
    
    if (!(var_type %in% audited_types)) {
      incidents[[i]] <- tibble::tibble(
        source_table = source_table,
        variable = col_name,
        type = var_type,
        n_total = n_total,
        n_pre_na = n_pre_na,
        n_recoded = 0L,
        n_post_na = n_pre_na,
        pct_recoded = 0,
        audit_status = "unknown_type"
      )
      next
    }
    
    # Apply audit rule
    out_of_domain <- rep(FALSE, n_total)
    valid_levels <- character(0L)
    
    if (var_type %in% c("binary", "factor")) {
      valid_levels <- dict_entry$domain_levels[[1]]
      if (length(valid_levels) > 0) {
        # Build a comparison universe that handles three encodings:
        #   - literal character form ("01", "Yes")
        #   - leading-zero-stripped form ("1" matches "01")
        #   - numeric-as-string form (1L -> "1")
        char_values <- as.character(column_values)
        valid_stripped <- stringr::str_remove(valid_levels, "^0+(?=[0-9])")
        valid_universe <- unique(c(valid_levels, valid_stripped))
        out_of_domain <- !is.na(char_values) & !(char_values %in% valid_universe)
      }
    } else {
      # numeric
      lo <- dict_entry$domain_min[[1]]
      hi <- dict_entry$domain_max[[1]]
      if (!is.na(lo) && !is.na(hi)) {
        numeric_values <- suppressWarnings(as.numeric(column_values))
        out_of_domain <- !is.na(numeric_values) &
          (numeric_values < lo | numeric_values > hi)
      }
    }
    
    n_recoded <- sum(out_of_domain)
    
    if (n_recoded > 0) {
      data_audited[[col_name]][out_of_domain] <- NA
    }
    
    # Type coercion: align column class with dictionary declaration
    if (var_type %in% c("binary", "factor") && length(valid_levels) > 0) {
      coerced_values <- as.character(data_audited[[col_name]])
      valid_stripped <- stringr::str_remove(valid_levels, "^0+(?=[0-9])")
      stripped_to_literal <- stats::setNames(valid_levels, valid_stripped)
      to_remap <- !is.na(coerced_values) &
        coerced_values %in% names(stripped_to_literal) &
        !(coerced_values %in% valid_levels)
      coerced_values[to_remap] <- stripped_to_literal[coerced_values[to_remap]]
      data_audited[[col_name]] <- factor(coerced_values, levels = valid_levels)
    } else if (var_type == "numeric_integer") {
      data_audited[[col_name]] <- suppressWarnings(
        as.integer(data_audited[[col_name]])
      )
    } else if (var_type == "numeric_continuous") {
      data_audited[[col_name]] <- suppressWarnings(
        as.numeric(data_audited[[col_name]])
      )
    }
    
    incidents[[i]] <- tibble::tibble(
      source_table = source_table,
      variable = col_name,
      type = var_type,
      n_total = n_total,
      n_pre_na = n_pre_na,
      n_recoded = as.integer(n_recoded),
      n_post_na = n_pre_na + as.integer(n_recoded),
      pct_recoded = round(100 * n_recoded / n_total, 3),
      audit_status = ifelse(n_recoded > 0, "recoded", "clean")
    )
  }
  
  incidents_summary <- dplyr::bind_rows(incidents) |>
    dplyr::arrange(dplyr::desc(n_recoded), variable)
  
  list(
    data = data_audited,
    incidents_summary = incidents_summary,
    source_table = source_table
  )
}

# ==============================================================================
# Ensure Output Directory Exists
# ==============================================================================
# Verifies that a target directory exists before writing outputs to it, and
# creates it (including any missing parent directories) when absent, so that
# write_parquet(), export(), and saveRDS() calls find their target directory
# when scripts run in isolation, outside the orchestrator that provisions the
# full processed-data structure.
#
# Arguments:
#   path - Character string with the target directory path, typically built
#          with here() (e.g., here("01_data", "02_processed", "transfer"))
#
# Returns:
#   The input path, invisibly. Called for its side effect of creating the
#   directory when it does not exist.
#
# Details:
#   - Uses recursive = TRUE to create intermediate parent directories
#   - Idempotent: does nothing when the directory already exists
#   - showWarnings = FALSE suppresses the warning emitted on race conditions
#     when the directory is created between the check and the create call
#
# Example:
#   # Guarantee the transfer directory before saving parquet outputs
#   ensure_output_dir(here("01_data", "02_processed", "transfer"))
#   write_parquet(df_household_intake, here(
#     "01_data", "02_processed", "transfer", "01_02_household_intake_total.parquet"
#   ))
# ==============================================================================
ensure_output_dir <- function(path) {
  
  # Validate input
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("'path' must be a single, non-missing, non-empty character string.")
  }
  
  # Create directory and parents when absent
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Fail early if creation was unsuccessful
  if (!dir.exists(path)) {
    stop("Output directory could not be created: ", path)
  }
  
  invisible(path)
}

# ==============================================================================
# 7. SCENARIO SIMULATION & MARKET MODELING
# ==============================================================================

# ==============================================================================
# Compute Sigmoid Conversion Curve for Continuous Adoption
# ==============================================================================
# Computes the per-farmer conversion ratio r_i(p) for the continuous adoption
# model. The curve is defined on [p_entry, 1] with r(p_entry) = floor_val and
# r(1) = 1 by construction. Outside [p_entry, 1] the conversion is 0 (farmer
# not yet entered the ranking).
#
# The sigmoid is parameterised as a rescaled logistic function on the
# normalised coordinate u = (p - p_entry) / (1 - p_entry), mapped to
# [floor_val, 1] so that the boundary conditions are satisfied exactly.
#
# Arguments:
#   p         - Numeric vector. National production coverage level(s) at
#               which to evaluate the curve. Values in [0, 1].
#   p_entry   - Numeric vector (same length as p, or scalar). Per-farmer
#               production coverage level at which the farmer enters the
#               ranking.
#   floor_val - Numeric scalar. Conversion ratio at the moment of entry
#               (p = p_entry). Must be in [0, 1].
#   k         - Numeric scalar. Steepness of the sigmoid. Higher values
#               produce sharper transitions from floor to 1.
#
# Returns:
#   Numeric vector (same length as p) with conversion ratios in [0, 1].
#   Returns 0 for p < p_entry.
#
# Details:
#   The raw logistic is computed as 1 / (1 + exp(-k * (u - 0.5))). This is
#   then linearly rescaled so that sigmoid_curve(p_entry) = floor_val and
#   sigmoid_curve(1) = 1. The rescaling uses the raw logistic evaluated at
#   u = 0 and u = 1 as anchoring points. A guard against (1 - p_entry) = 0
#   prevents division by zero for farmers who enter at full coverage.
#
# Example:
#   # Evaluate conversion at 50% production coverage for a farmer entering
#   # at 10%
#   r <- sigmoid_curve(p = 0.50, p_entry = 0.10, floor_val = 0.85, k = 12)
# ==============================================================================
sigmoid_curve <- function(p, p_entry, floor_val, k) {
  
  u <- (p - p_entry) / pmax(1 - p_entry, 1e-6)
  
  raw  <- 1 / (1 + exp(-k * (u - 0.5)))
  raw0 <- 1 / (1 + exp(-k * (0 - 0.5)))
  raw1 <- 1 / (1 + exp(-k * (1 - 0.5)))
  
  scaled <- floor_val + (1 - floor_val) * (raw - raw0) / (raw1 - raw0)
  
  dplyr::if_else(p < p_entry, 0, pmin(1, pmax(floor_val, scaled)))
}

# ==============================================================================
# Apply Per-Profile Continuous Adoption Curve
# ==============================================================================
# Dispatches the sigmoid conversion curve to each farmer based on their
# behavioural profile (S = small, M = medium, L = large). Each profile has
# its own floor value and steepness coefficient, producing qualitatively
# distinct adoption behaviours: near-binary for small farmers, gradual for
# large farmers.
#
# Arguments:
#   p         - Numeric vector. National production coverage level(s).
#   p_entry   - Numeric vector (same length as p). Per-farmer entry point.
#   profile   - Character vector (same length as p). Profile assignment,
#               one of "S", "M", "L".
#   floor_S   - Numeric scalar. Floor for profile S (default 0.85).
#   floor_M   - Numeric scalar. Floor for profile M (default 0.40).
#   floor_L   - Numeric scalar. Floor for profile L (default 0.10).
#   k_S       - Numeric scalar. Steepness for profile S (default 12).
#   k_M       - Numeric scalar. Steepness for profile M (default 8).
#   k_L       - Numeric scalar. Steepness for profile L (default 4).
#
# Returns:
#   Numeric vector (same length as p) with conversion ratios in [0, 1].
#
# Details:
#   The function is fully vectorised. Profile assignment determines which
#   set of parameters (floor, k) is used for each element. The convergence
#   property r_i(1) = 1 is enforced by construction for all profiles via
#   sigmoid_curve().
#
# Example:
#   r_i <- apply_adoption_curve(
#     p = 0.30,
#     p_entry = df$p_entry,
#     profile = df$profile_entry,
#     floor_S = 0.95, floor_M = 0.50, floor_L = 0.05,
#     k_S = 12, k_M = 8, k_L = 2
#   )
# ==============================================================================
apply_adoption_curve <- function(p, p_entry, profile,
                                 floor_S = 0.85, floor_M = 0.40, floor_L = 0.10,
                                 k_S = 12, k_M = 8, k_L = 4) {
  
  dplyr::case_when(
    profile == "S" ~ sigmoid_curve(p, p_entry, floor_S, k_S),
    profile == "M" ~ sigmoid_curve(p, p_entry, floor_M, k_M),
    profile == "L" ~ sigmoid_curve(p, p_entry, floor_L, k_L)
  )
}

# ==============================================================================
# Compute Weighted Trimmed Mean (P5-P95)
# ==============================================================================
# Computes a survey-weighted trimmed mean using the P5-P95 convention. The
# trimming bounds are computed within the supplied (values, weights) pair,
# so they reflect the distribution of the specific cohort rather than a
# global reference.
#
# This function is used throughout the Likely Adopters calibration framework
# and downstream scenario modules for computing cohort-level land and
# production metrics consistent with the reporting convention applied in
# script 02_03 (farmer segmentation).
#
# Arguments:
#   values  - Numeric vector of observations.
#   weights - Numeric vector of survey weights (same length as values).
#
# Returns:
#   Numeric scalar: the weighted mean of observations within the [P5, P95]
#   interval. Returns NA_real_ if fewer than 2 valid observations remain
#   after removing NAs and zero/negative weights, or if no observations
#   fall within the trimming bounds.
#
# Details:
#   - Observations with NA values, NA weights, or non-positive weights are
#     excluded before quantile computation.
#   - Quantiles are computed via Hmisc::wtd.quantile() using the supplied
#     weights, ensuring consistency with the survey-weighted distributions
#     used elsewhere in the framework.
#   - The function does not modify the input vectors.
#
# Example:
#   land_mean <- trimmed_mean_p5p95(
#     values  = df_cohort$land_wtcorn,
#     weights = df_cohort$weight_calibrated
#   )
# ==============================================================================
trimmed_mean_p5p95 <- function(values, weights) {
  
  # Check required package
  if (!requireNamespace("Hmisc", quietly = TRUE)) {
    stop("Package 'Hmisc' is required but not installed.")
  }
  
  ok <- !is.na(values) & !is.na(weights) & weights > 0
  if (sum(ok) < 2) return(NA_real_)
  
  v <- values[ok]
  w <- weights[ok]
  
  q <- Hmisc::wtd.quantile(v, weights = w, probs = c(0.05, 0.95))
  keep <- v >= q[[1]] & v <= q[[2]]
  
  if (!any(keep)) return(NA_real_)
  
  sum(v[keep] * w[keep]) / sum(w[keep])
}

# ==============================================================================
# Compute Departmental Z-Score for a Numeric Variable
# ==============================================================================
# Adds a column with the within-department z-score of a numeric variable,
# computed using survey-weighted mean and variance. This matches the
# standardisation pattern used in script 02_03 (farmer segmentation) for
# constructing segment-specific z-scores.
#
# Arguments:
#   df         - Data frame containing the variable to standardise.
#   var        - Character string. Name of the numeric column to z-score.
#   new_name   - Character string. Name of the new z-score column to create.
#   weight_var - Character string. Name of the survey weight column
#                (default "weight_calibrated").
#   dept_var   - Character string. Name of the department column
#                (default "departamento").
#
# Returns:
#   Data frame identical to the input with one additional column (new_name)
#   containing the within-department z-scores.
#
# Details:
#   - Weighted mean via base::weighted.mean() with na.rm = TRUE.
#   - Weighted variance via Hmisc::wtd.var() with na.rm = TRUE.
#   - Departments with zero variance produce NaN z-scores (division by zero);
#     this is the expected behaviour and is handled downstream.
#   - Intermediate columns (.mu, .sd) are created and removed within the
#     function body.
#
# Example:
#   df_farmers <- add_dept_zscore(
#     df       = df_farmers,
#     var      = "yield_wtcorn_qq",
#     new_name = "yield_zscore_risk"
#   )
# ==============================================================================
add_dept_zscore <- function(df, var, new_name,
                            weight_var = "weight_calibrated",
                            dept_var   = "departamento") {
  
  # Check required package
  if (!requireNamespace("Hmisc", quietly = TRUE)) {
    stop("Package 'Hmisc' is required but not installed.")
  }
  
  dept_stats <- df |>
    dplyr::summarise(
      .mu = weighted.mean(.data[[var]], w = .data[[weight_var]],
                          na.rm = TRUE),
      .sd = sqrt(Hmisc::wtd.var(.data[[var]],
                                weights = .data[[weight_var]],
                                na.rm = TRUE)),
      .by = dplyr::all_of(dept_var)
    )
  
  df |>
    dplyr::left_join(dept_stats, by = dept_var) |>
    dplyr::mutate("{new_name}" := (.data[[var]] - .mu) / .sd) |>
    dplyr::select(-.mu, -.sd)
}

# ==============================================================================
# Score Segment Component (Categorical)
# ==============================================================================
# Computes the segment-priority score component of the Likely Adopters
# composite score. Maps each farmer's segment label to a cardinal point
# value held in a named vector. Used by both the calibration script
# (02_05) and the application script (02_06x).
#
# Arguments:
#   segment - Character vector of segment labels with values in
#             {"Low", "OPV/Criollo", "Mid", "High"}
#   points  - Named numeric vector with elements "Low", "OPV", "Mid",
#             "High" assigning the cardinal score per segment
#
# Returns:
#   Integer vector of length(segment) with the assigned scores. Returns
#   0L for any segment label not present in the names of `points`.
#
# Example:
#   pts <- c(Low = 100, OPV = 45, Mid = 70, High = 85)
#   score_segment_fn(df$segment_final, pts)
# ==============================================================================
score_segment_fn <- function(segment, points) {
  
  # Check required packages are installed
  required_packages <- c("dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  dplyr::case_when(
    segment == "Low"         ~ as.integer(points[["Low"]]),
    segment == "OPV/Criollo" ~ as.integer(points[["OPV"]]),
    segment == "Mid"         ~ as.integer(points[["Mid"]]),
    segment == "High"        ~ as.integer(points[["High"]]),
    TRUE                     ~ 0L
  )
}

# ==============================================================================
# Score Economic Benefit Component (Percentile-Based)
# ==============================================================================
# Computes the economic-benefit score component of the Likely Adopters
# composite score. Bins farmers into five tiers (100/75/50/25/0) using
# three percentile breakpoints over a continuous economic variable
# (typically `income_increment`). The fifth tier (0) captures farmers
# with non-positive value, isolating the strict negative-impact subset
# from the standard positive band.
#
# Arguments:
#   x        - Numeric vector of the economic variable (e.g.,
#              income_increment in Quetzales)
#   bp_probs - Numeric vector of length 3 with percentile breakpoints
#              in [0, 1], strictly increasing. Default c(0.25, 0.50, 0.75)
#
# Returns:
#   Integer vector of length(x) with values in {0L, 25L, 50L, 75L, 100L}
#
# Example:
#   score_economic_fn(df$income_increment, c(0.20, 0.40, 0.60))
# ==============================================================================
score_economic_fn <- function(x, bp_probs = c(0.25, 0.50, 0.75)) {
  
  # Check required packages are installed
  required_packages <- c("dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  q <- quantile(x, probs = bp_probs, na.rm = TRUE)
  dplyr::case_when(
    x >= q[[3]] ~ 100L,
    x >= q[[2]] ~  75L,
    x >= q[[1]] ~  50L,
    x >= 0      ~  25L,
    TRUE        ~   0L
  )
}

# ==============================================================================
# Score Improvement Potential Component (Inverse Percentile)
# ==============================================================================
# Computes the improvement-potential score component of the Likely
# Adopters composite score. The component operates in inverse direction:
# lower values of the input variable receive higher scores, on the
# rationale that farmers with lower current input expenditure per unit
# area have larger margin for productivity improvement under the
# biofortified seed configuration.
#
# Arguments:
#   x        - Numeric vector of the inverse-direction variable
#              (typically `exp_total_per_mz_annual`)
#   bp_probs - Numeric vector of length 4 with percentile breakpoints
#              in [0, 1], strictly increasing. Default
#              c(0.25, 0.50, 0.75, 0.90)
#
# Returns:
#   Integer vector of length(x) with values in {0L, 25L, 50L, 75L, 100L}
#
# Example:
#   score_improvement_fn(df$exp_total_per_mz_annual, c(0.30, 0.60, 0.80, 0.95))
# ==============================================================================
score_improvement_fn <- function(x, bp_probs = c(0.25, 0.50, 0.75, 0.90)) {
  
  # Check required packages are installed
  required_packages <- c("dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  q <- quantile(x, probs = bp_probs, na.rm = TRUE)
  dplyr::case_when(
    x <= q[[1]] ~ 100L,
    x <= q[[2]] ~  75L,
    x <= q[[3]] ~  50L,
    x <= q[[4]] ~  25L,
    TRUE        ~   0L
  )
}

# ==============================================================================
# Score Scale Potential Component (Percentile-Based)
# ==============================================================================
# Computes the scale-potential score component of the Likely Adopters
# composite score. Bins farmers into four tiers (100/75/50/25) using
# three percentile breakpoints over the per-farmer entry land
# commitment (`land_entry_size`). The component has four bands: scale
# is a non-negative magnitude by construction.
#
# Arguments:
#   x        - Numeric vector of the scale variable (typically
#              `land_entry_size` in manzanas)
#   bp_probs - Numeric vector of length 3 with percentile breakpoints
#              in [0, 1], strictly increasing. Default c(0.25, 0.50, 0.75)
#
# Returns:
#   Integer vector of length(x) with values in {25L, 50L, 75L, 100L}
#
# Example:
#   score_scale_fn(df$land_entry_size, c(0.25, 0.50, 0.75))
# ==============================================================================
score_scale_fn <- function(x, bp_probs = c(0.25, 0.50, 0.75)) {
  
  # Check required packages are installed
  required_packages <- c("dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  q <- quantile(x, probs = bp_probs, na.rm = TRUE)
  dplyr::case_when(
    x >= q[[3]] ~ 100L,
    x >= q[[2]] ~  75L,
    x >= q[[1]] ~  50L,
    TRUE        ~  25L
  )
}

# ==============================================================================
# Construct Age-Score Function (Three-Axis Factory)
# ==============================================================================
# Returns a closure that evaluates the farmer-age score component on a
# [0, 100] scale. The score has a central plateau of 100 around
# peak_center with half-width peak_width, and four flanking bands
# (near_below, far_below, near_above, far_above) whose secondary
# scores are governed by the profile argument. Bands of width 10 years
# are placed symmetrically around the central plateau; ages outside
# the four flanking bands receive the "extreme" score.
#
# Arguments:
#   peak_center - Numeric scalar: centre of the score-100 plateau
#                 (e.g., 45, 50, 55)
#   peak_width  - Numeric scalar: half-width of the score-100 plateau
#                 (e.g., 5, 10)
#   profile     - Character scalar selecting the distribution of
#                 secondary scores across flanking bands. One of:
#                 "decreasing", "young_priority", "elder_priority",
#                 "neutral_edges"
#
# Returns:
#   A closure of one argument (farmer_age) that returns an integer
#   vector of length(farmer_age) with the assigned scores.
#
# Example:
#   score_age_fn <- make_score_age(peak_center = 55, peak_width = 10,
#                                  profile = "neutral_edges")
#   score_age_fn(df$farmer_age)
# ==============================================================================
make_score_age <- function(peak_center, peak_width, profile) {
  
  # Check required packages are installed
  required_packages <- c("dplyr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  function(farmer_age) {
    peak_lo    <- peak_center - peak_width
    peak_hi    <- peak_center + peak_width
    near_lo_lo <- peak_lo - 10
    far_lo_lo  <- peak_lo - 20
    near_hi_hi <- peak_hi + 10
    far_hi_hi  <- peak_hi + 20
    
    pts <- switch(profile,
                  "decreasing"     = c(near_below = 80L, far_below = 60L,
                                       near_above = 80L, far_above = 60L,
                                       extreme = 50L),
                  "young_priority" = c(near_below = 85L, far_below = 70L,
                                       near_above = 60L, far_above = 40L,
                                       extreme = 50L),
                  "elder_priority" = c(near_below = 60L, far_below = 40L,
                                       near_above = 85L, far_above = 70L,
                                       extreme = 50L),
                  "neutral_edges"  = c(near_below = 70L, far_below = 55L,
                                       near_above = 70L, far_above = 55L,
                                       extreme = 50L)
    )
    
    dplyr::case_when(
      farmer_age >= peak_lo    & farmer_age <  peak_hi    ~ 100L,
      farmer_age >= near_lo_lo & farmer_age <  peak_lo    ~ pts[["near_below"]],
      farmer_age >= far_lo_lo  & farmer_age <  near_lo_lo ~ pts[["far_below"]],
      farmer_age >= peak_hi    & farmer_age <  near_hi_hi ~ pts[["near_above"]],
      farmer_age >= near_hi_hi & farmer_age <  far_hi_hi  ~ pts[["far_above"]],
      TRUE                                                ~ pts[["extreme"]]
    )
  }
}

# ==============================================================================
# Weighted Quantiles (Survey-Weighted)
# ==============================================================================
# Computes survey-weighted quantiles of a numeric vector by linear
# interpolation on the cumulative weight distribution. The
# implementation orders the values, accumulates the weights, and
# selects the values whose cumulative weight share is closest to each
# requested probability. Used as a helper for the P1-P99 normalisation
# of the risk-aversion drivers in the Likely Adopters model.
#
# Arguments:
#   x     - Numeric vector of values
#   w     - Numeric vector of non-negative weights, same length as x
#   probs - Numeric vector of probabilities in [0, 1]
#
# Returns:
#   Numeric vector of length(probs) with the weighted quantile values
#
# Example:
#   weighted_quantiles(df$income, df$weight_calibrated, c(0.01, 0.5, 0.99))
# ==============================================================================
weighted_quantiles <- function(x, w, probs) {
  
  # Check required packages are installed
  required_packages <- c("purrr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed."))
    }
  }
  
  o   <- order(x)
  xs  <- x[o]
  ws  <- w[o]
  cw  <- cumsum(ws) / sum(ws)
  purrr::map_dbl(probs, ~ xs[which.min(abs(cw - .x))])
}

# ==============================================================================
# Scale Vector via P1-P99 (Survey-Weighted)
# ==============================================================================
# Normalises a numeric vector to a centred scale via survey-weighted
# percentiles P1 and P99. The output is centred at the weighted median
# (subtracted) and scaled by half the P1-P99 range, producing a
# dimensionless variable bounded approximately in [-1, +1] for inliers.
# Returns a zero vector if the P1-P99 range collapses to zero. Used as
# the absolute-value variant for the risk-aversion drivers in the
# Likely Adopters model.
#
# Arguments:
#   x - Numeric vector to be scaled
#   w - Numeric vector of non-negative weights, same length as x
#
# Returns:
#   Numeric vector of length(x) with the scaled values
#
# Example:
#   scale_p1_p99(df$yield_wtcorn_qq, df$weight_calibrated)
# ==============================================================================
scale_p1_p99 <- function(x, w) {
  
  qs <- weighted_quantiles(x, w, c(0.01, 0.50, 0.99))
  half_range <- (qs[3] - qs[1]) / 2
  if (half_range <= 0) return(rep(0, length(x)))
  (x - qs[2]) / half_range
}

# ==============================================================================
# Redistribute Excess Production from Origin to Receivers
# ==============================================================================
# Redistributes biofortified maize production excess from a single origin
# department to its designated receiver departments, splitting equally
# among all receivers regardless of their current coverage.
#
# Arguments:
#   df        - Data frame with columns: departamento, annual_consumption,
#               bio_production
#   origin    - Character. Name of origin department with excess production
#   receivers - Character vector. Names of departments that receive from origin
#
# Returns:
#   Data frame with updated columns:
#     - bio_production: updated with redistributed amounts
#     - coverage: recalculated as bio_production / annual_consumption
#     - excess: recalculated excess after redistribution
#
# Details:
#   The function distributes excess equally among all receivers, regardless
#   of their unfulfilled demand. Receivers may exceed 100% coverage, becoming
#   secondary donors when processed later in the topological order.
#   The origin department's production is capped at its own consumption.
#
# Example:
#   df_updated <- redistribute_from_origin(
#     df = df_coverage,
#     origin = "Peten",
#     receivers = c("Alta Verapaz", "Izabal", "Quiché")
#   )
# ==============================================================================
redistribute_from_origin <- function(df, origin, receivers) {
  
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("Argument 'df' must be a data frame.")
  }
  
  required_cols <- c("departamento", "annual_consumption", "bio_production")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  if (!is.character(origin) || length(origin) != 1) {
    stop("Argument 'origin' must be a single character string.")
  }
  
  if (!origin %in% df$departamento) {
    stop(paste("Origin department", origin, "not found in data."))
  }
  
  # If no receivers defined, return unchanged
  if (length(receivers) == 0) {
    return(df)
  }
  
  # Validate receivers exist in data
  missing_receivers <- setdiff(receivers, df$departamento)
  if (length(missing_receivers) > 0) {
    warning(paste("Receivers not found in data (ignored):", 
                  paste(missing_receivers, collapse = ", ")))
    receivers <- intersect(receivers, df$departamento)
  }
  
  if (length(receivers) == 0) {
    return(df)
  }
  
  # Extract origin's current values
  origin_row <- df |> dplyr::filter(departamento == origin)
  origin_production <- origin_row$bio_production
  origin_consumption <- origin_row$annual_consumption
  
  # Calculate excess
  excess <- max(origin_production - origin_consumption, 0)
  
  # If no excess, nothing to redistribute
  if (excess <= 0) {
    return(df)
  }
  
  # Calculate equal allocation per receiver
  n_receivers <- length(receivers)
  allocation_per_receiver <- excess / n_receivers
  
  # Apply redistribution
  df_result <- df |>
    dplyr::mutate(
      bio_production = dplyr::case_when(
        # Origin loses its excess (capped at consumption)
        departamento == origin ~ annual_consumption,
        # Receivers gain equal share
        departamento %in% receivers ~ bio_production + allocation_per_receiver,
        # Others unchanged
        TRUE ~ bio_production
      ),
      # Recalculate coverage and excess
      coverage = bio_production / annual_consumption,
      excess = pmax(bio_production - annual_consumption, 0)
    )
  
  return(df_result)
}

# ==============================================================================
# Apply Complete Excess Redistribution Cascade
# ==============================================================================
# Applies the full redistribution cascade across all departments following
# a topological processing order. Each department redistributes its excess
# to designated receivers before the next department is processed.
#
# Arguments:
#   df           - Data frame with columns: departamento, annual_consumption,
#                  bio_production
#   adjacency_df - Data frame with columns: departamento (origin), receivers
#                  (list-column of receiver department names)
#   order        - Character vector. Topological order for processing
#                  departments (from earliest saturation to latest)
#
# Returns:
#   Data frame with all excess redistributed through the network:
#     - bio_production: final production after all redistributions
#     - coverage: final coverage ratio
#     - excess: remaining excess (should be zero for most departments)
#
# Details:
#   The function iterates through departments in the specified order,
#   calling redistribute_from_origin() for each. The order is critical:
#   departments that saturate first should be processed first so their
#   excess can cascade through the network to deficit departments.
#
# Example:
#   df_final <- redistribute_excess_cascade(
#     df = df_coverage,
#     adjacency_df = df_adjacency,
#     order = topological_order
#   )
# ==============================================================================
redistribute_excess_cascade <- function(df, adjacency_df, order) {
  
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("Argument 'df' must be a data frame.")
  }
  
  if (!is.data.frame(adjacency_df)) {
    stop("Argument 'adjacency_df' must be a data frame.")
  }
  
  if (!all(c("departamento", "receivers") %in% names(adjacency_df))) {
    stop("Argument 'adjacency_df' must have columns 'departamento' and 'receivers'.")
  }
  
  if (!is.character(order)) {
    stop("Argument 'order' must be a character vector.")
  }
  
  # Initialize result with required columns
  df_result <- df |>
    dplyr::mutate(
      departamento = as.character(departamento),
      coverage = bio_production / annual_consumption,
      excess = pmax(bio_production - annual_consumption, 0)
    )
  
  # Process each department in order
  for (current_origin in order) {
    
    # Get receivers for this origin
    receivers_row <- adjacency_df |>
      dplyr::filter(departamento == current_origin)
    
    if (nrow(receivers_row) == 0) {
      # Origin not in adjacency matrix, skip
      next
    }
    
    current_receivers <- receivers_row$receivers[[1]]
    
    # Apply redistribution from this origin
    df_result <- redistribute_from_origin(
      df = df_result,
      origin = current_origin,
      receivers = current_receivers
    )
  }
  
  return(df_result)
}

# ==============================================================================
# Redistribute Excess Production Using Gravity Model
# ==============================================================================
# Redistributes biofortified maize production surplus from all departments
# with excess to all departments with deficit, in a single simultaneous pass,
# weighted by gravity (inverse travel time). Iterates until convergence.
#
# The formulation follows Anderson & van Wincoop (2003), analogous to IMPLAN
# inter-regional commodity flow estimation.
#
# Arguments:
#   df               - Data frame with columns: departamento, annual_consumption,
#                      bio_production
#   travel_time_df   - Data frame in long format with columns: origin,
#                      destination, gravity_weight (pre-computed as
#                      1 / travel_time^beta)
#   max_iterations   - Integer. Maximum redistribution passes (default: 50).
#                      Convergence typically occurs in 2-5 iterations.
#   tolerance        - Numeric. Convergence threshold in total excess change
#                      between iterations (default: 1, i.e. 1 quintal)
#
# Returns:
#   Data frame with columns:
#     - departamento: department name
#     - bio_production: final production after redistribution
#     - annual_consumption: unchanged from input
#     - coverage: final coverage ratio (bio_production / annual_consumption)
#     - excess: remaining excess after redistribution (should be minimal)
#     - iterations: number of passes until convergence
#
# Details:
#   For each department i with excess, the flow to deficit department j is:
#
#     flow_ij = excess_i * w_ij * deficit_j / sum_k(w_ik * deficit_k)
#
#   where w_ij = gravity_weight (pre-computed as 1/t_ij^beta) and the sum
#   is over all departments k with deficit > 0. The travel time matrix is
#   directional (asymmetric): flow from i to j uses w(i->j), not w(j->i).
#
#   After each pass, departments that received more than their deficit
#   become new donors in the next pass. Iteration continues until total
#   redistributable excess falls below the tolerance threshold.
#
# Example:
#   df_redistributed <- redistribute_gravity(
#     df = df_coverage,
#     travel_time_df = df_gravity_weights,
#     max_iterations = 50,
#     tolerance = 1
#   )
# ==============================================================================
redistribute_gravity <- function(df,
                                 travel_time_df,
                                 max_iterations = 50,
                                 tolerance = 1) {
  
  # --- Input validation -------------------------------------------------------
  if (!is.data.frame(df)) {
    stop("Argument 'df' must be a data frame.")
  }
  
  required_cols_df <- c("departamento", "annual_consumption", "bio_production")
  missing_cols_df <- setdiff(required_cols_df, names(df))
  if (length(missing_cols_df) > 0) {
    stop(paste("Missing required columns in df:",
               paste(missing_cols_df, collapse = ", ")))
  }
  
  if (!is.data.frame(travel_time_df)) {
    stop("Argument 'travel_time_df' must be a data frame.")
  }
  
  required_cols_tt <- c("origin", "destination", "gravity_weight")
  missing_cols_tt <- setdiff(required_cols_tt, names(travel_time_df))
  if (length(missing_cols_tt) > 0) {
    stop(paste("Missing required columns in travel_time_df:",
               paste(missing_cols_tt, collapse = ", ")))
  }
  
  # --- Initialize working data ------------------------------------------------
  df_result <- df |>
    dplyr::mutate(
      departamento = as.character(departamento),
      coverage = bio_production / annual_consumption,
      excess = pmax(bio_production - annual_consumption, 0)
    )
  
  # --- Iterative redistribution -----------------------------------------------
  iteration <- 0
  
  for (i in seq_len(max_iterations)) {
    
    # Identify donors (excess > 0) and receivers (deficit > 0)
    donors <- df_result |>
      dplyr::filter(excess > 0) |>
      dplyr::pull(departamento)
    
    receivers <- df_result |>
      dplyr::filter(coverage < 1) |>
      dplyr::pull(departamento)
    
    # If no donors or no receivers, redistribution is complete
    if (length(donors) == 0 || length(receivers) == 0) {
      break
    }
    
    # Track total excess before this pass
    total_excess_before <- sum(df_result$excess)
    
    # For each donor, compute gravity-weighted flows to all receivers
    for (donor_dept in donors) {
      
      donor_excess <- df_result$excess[df_result$departamento == donor_dept]
      
      if (donor_excess <= 0) next
      
      # Get gravity weights from this donor to all current receivers
      df_weights <- travel_time_df |>
        dplyr::filter(
          origin == donor_dept,
          destination %in% receivers
        ) |>
        dplyr::select(destination, gravity_weight)
      
      # Get current deficits for receivers
      df_receiver_deficit <- df_result |>
        dplyr::filter(departamento %in% receivers) |>
        dplyr::transmute(
          destination = departamento,
          deficit = pmax(annual_consumption - bio_production, 0)
        )
      
      # Join weights with deficits
      df_flow <- df_weights |>
        dplyr::inner_join(df_receiver_deficit, by = "destination") |>
        dplyr::filter(deficit > 0, gravity_weight > 0)
      
      if (nrow(df_flow) == 0) next
      
      # Compute weighted shares: w_ij * deficit_j / sum(w_ik * deficit_k)
      df_flow <- df_flow |>
        dplyr::mutate(
          weighted_pull = gravity_weight * deficit,
          share = weighted_pull / sum(weighted_pull),
          allocated = donor_excess * share,
          # Cap allocation at receiver's deficit (no over-filling)
          allocated_capped = pmin(allocated, deficit)
        )
      
      # Total actually distributed (may be less than donor's excess if caps bind)
      total_distributed <- sum(df_flow$allocated_capped)
      
      # Update donor: reduce production by amount distributed
      df_result <- df_result |>
        dplyr::mutate(
          bio_production = dplyr::if_else(
            departamento == donor_dept,
            bio_production - total_distributed,
            bio_production
          )
        )
      
      # Update receivers: add allocated amounts
      for (r in seq_len(nrow(df_flow))) {
        recv_dept <- df_flow$destination[r]
        recv_amount <- df_flow$allocated_capped[r]
        df_result <- df_result |>
          dplyr::mutate(
            bio_production = dplyr::if_else(
              departamento == recv_dept,
              bio_production + recv_amount,
              bio_production
            )
          )
      }
    }
    
    # Recalculate coverage and excess after this pass
    df_result <- df_result |>
      dplyr::mutate(
        coverage = bio_production / annual_consumption,
        excess = pmax(bio_production - annual_consumption, 0)
      )
    
    iteration <- i
    
    # Check convergence: total excess change
    total_excess_after <- sum(df_result$excess)
    excess_change <- abs(total_excess_before - total_excess_after)
    
    if (excess_change < tolerance) {
      break
    }
  }
  
  # Add iteration count
  df_result <- df_result |>
    dplyr::mutate(iterations = iteration)
  
  return(df_result)
}

