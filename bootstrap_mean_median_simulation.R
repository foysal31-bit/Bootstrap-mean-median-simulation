# A BOOTSTRAP SIMULATION STUDY OF THE EFFICIENCY AND
# ROBUSTNESS OF THE MEAN AND MEDIAN
#
# Expanded from the original STAT 653 coursework project
# Base R version




# 0. PROJECT SETTINGS


# Detailed bootstrap repetitions
B_detailed <- 5000

# Monte Carlo repetitions for estimator comparisons
M_simulation <- 1000

# Coverage simulation settings
M_coverage <- 300
B_coverage <- 500

# True center of the uncontaminated population
true_value <- 4

# Create folders for saved tables and figures
if (!dir.exists("results")) {
  dir.create("results")
}

if (!dir.exists("figures")) {
  dir.create("figures")
}



# 1. BOOTSTRAP ANALYSIS FUNCTION


bootest <- function(x,
                    B = 5000,
                    conf_level = 0.95,
                    true_value = 4,
                    scenario_name = "Sample",
                    make_plots = TRUE,
                    save_plot = FALSE) {

  alpha <- 1 - conf_level
  n <- length(x)

  bootstrap_means <- rep(0, B)
  bootstrap_medians <- rep(0, B)

  # Generate bootstrap samples
  for (i in 1:B) {

    samp <- sample(
      x,
      n,
      replace = TRUE
    )

    bootstrap_means[i] <- mean(samp)
    bootstrap_medians[i] <- median(samp)
  }

  # Original sample estimates
  sample_mean <- mean(x)
  sample_median <- median(x)

  # Average bootstrap estimates
  bootstrap_average_mean <- mean(bootstrap_means)
  bootstrap_average_median <- mean(bootstrap_medians)

  # Bootstrap-estimated bias
  bootstrap_bias_mean <-
    bootstrap_average_mean - sample_mean

  bootstrap_bias_median <-
    bootstrap_average_median - sample_median

  # Percentile bootstrap confidence interval for mean
  lower_mean <- quantile(
    bootstrap_means,
    alpha / 2,
    na.rm = TRUE
  )[[1]]

  upper_mean <- quantile(
    bootstrap_means,
    1 - alpha / 2,
    na.rm = TRUE
  )[[1]]

  # Percentile bootstrap confidence interval for median
  lower_median <- quantile(
    bootstrap_medians,
    alpha / 2,
    na.rm = TRUE
  )[[1]]

  upper_median <- quantile(
    bootstrap_medians,
    1 - alpha / 2,
    na.rm = TRUE
  )[[1]]

  # Bootstrap variance
  variance_mean <- var(bootstrap_means)
  variance_median <- var(bootstrap_medians)

  # Bootstrap standard error
  standard_error_mean <- sqrt(variance_mean)
  standard_error_median <- sqrt(variance_median)

  # Relative efficiency:
  # Variance of median divided by variance of mean
  relative_efficiency <-
    variance_median / variance_mean

  # Squared error in this particular sample
  squared_error_mean <-
    (sample_mean - true_value)^2

  squared_error_median <-
    (sample_median - true_value)^2

  # Confidence interval widths
  confidence_width_mean <-
    upper_mean - lower_mean

  confidence_width_median <-
    upper_median - lower_median

  # Results table
  result_table <- data.frame(

    Estimator = c(
      "Mean",
      "Median"
    ),

    Sample_Estimate = c(
      sample_mean,
      sample_median
    ),

    Bootstrap_Average = c(
      bootstrap_average_mean,
      bootstrap_average_median
    ),

    Bootstrap_Bias = c(
      bootstrap_bias_mean,
      bootstrap_bias_median
    ),

    Bootstrap_Variance = c(
      variance_mean,
      variance_median
    ),

    Bootstrap_SE = c(
      standard_error_mean,
      standard_error_median
    ),

    CI_Lower = c(
      lower_mean,
      lower_median
    ),

    CI_Upper = c(
      upper_mean,
      upper_median
    ),

    CI_Width = c(
      confidence_width_mean,
      confidence_width_median
    ),

    Squared_Error_to_Target = c(
      squared_error_mean,
      squared_error_median
    )
  )

  cat("\n====================================================\n")
  cat("BOOTSTRAP RESULTS:", scenario_name, "\n")
  cat("====================================================\n")

  print(result_table)

  cat(
    "\nRelative Efficiency:",
    round(relative_efficiency, 4),
    "\n"
  )

  if (relative_efficiency > 1) {

    cat(
      "Variance interpretation:",
      "The mean has lower variance and is more efficient.\n"
    )

  } else if (relative_efficiency < 1) {

    cat(
      "Variance interpretation:",
      "The median has lower variance and is more efficient.\n"
    )

  } else {

    cat(
      "Variance interpretation:",
      "Both estimators have equal variance.\n"
    )
  }

  if (squared_error_mean < squared_error_median) {

    cat(
      "Single-sample error interpretation:",
      "The mean has smaller squared error.\n"
    )

  } else if (squared_error_median < squared_error_mean) {

    cat(
      "Single-sample error interpretation:",
      "The median has smaller squared error.\n"
    )

  } else {

    cat(
      "Single-sample error interpretation:",
      "Both estimators have equal squared error.\n"
    )
  }

  # Bootstrap plots
  if (make_plots == TRUE) {

    if (save_plot == TRUE) {

      file_name <- paste0(
        "figures/",
        gsub(
          " ",
          "_",
          tolower(scenario_name)
        ),
        "_bootstrap_distributions.png"
      )

      png(
        filename = file_name,
        width = 1200,
        height = 550
      )
    }

    par(mfrow = c(1, 2))

    # Bootstrap distribution of mean
    hist(
      bootstrap_means,
      breaks = 20,
      freq = FALSE,
      col = "lightblue",
      main = paste(
        "Bootstrap Mean:",
        scenario_name
      ),
      xlab = "Bootstrap Mean"
    )

    lines(
      density(bootstrap_means),
      col = "red",
      lwd = 2
    )

    abline(
      v = sample_mean,
      col = "blue",
      lwd = 2
    )

    abline(
      v = true_value,
      col = "darkgreen",
      lwd = 2,
      lty = 2
    )

    abline(
      v = c(
        lower_mean,
        upper_mean
      ),
      col = "purple",
      lwd = 2,
      lty = 3
    )

    legend(
      "topright",
      legend = c(
        "Sample Estimate",
        "Target Value",
        "Confidence Limits"
      ),
      col = c(
        "blue",
        "darkgreen",
        "purple"
      ),
      lty = c(
        1,
        2,
        3
      ),
      lwd = 2,
      cex = 0.75
    )

    # Bootstrap distribution of median
    hist(
      bootstrap_medians,
      breaks = 20,
      freq = FALSE,
      col = "lightyellow",
      main = paste(
        "Bootstrap Median:",
        scenario_name
      ),
      xlab = "Bootstrap Median"
    )

    lines(
      density(bootstrap_medians),
      col = "red",
      lwd = 2
    )

    abline(
      v = sample_median,
      col = "blue",
      lwd = 2
    )

    abline(
      v = true_value,
      col = "darkgreen",
      lwd = 2,
      lty = 2
    )

    abline(
      v = c(
        lower_median,
        upper_median
      ),
      col = "purple",
      lwd = 2,
      lty = 3
    )

    legend(
      "topright",
      legend = c(
        "Sample Estimate",
        "Target Value",
        "Confidence Limits"
      ),
      col = c(
        "blue",
        "darkgreen",
        "purple"
      ),
      lty = c(
        1,
        2,
        3
      ),
      lwd = 2,
      cex = 0.75
    )

    par(mfrow = c(1, 1))

    if (save_plot == TRUE) {
      dev.off()
    }
  }

  invisible(
    list(

      summary = result_table,

      sample_mean = sample_mean,
      sample_median = sample_median,

      bootstrap_average_mean =
        bootstrap_average_mean,

      bootstrap_average_median =
        bootstrap_average_median,

      bootstrap_bias_mean =
        bootstrap_bias_mean,

      bootstrap_bias_median =
        bootstrap_bias_median,

      var_mean = variance_mean,
      var_median = variance_median,

      se_mean = standard_error_mean,
      se_median = standard_error_median,

      ci_mean = c(
        lower_mean,
        upper_mean
      ),

      ci_median = c(
        lower_median,
        upper_median
      ),

      ci_width_mean =
        confidence_width_mean,

      ci_width_median =
        confidence_width_median,

      squared_error_mean =
        squared_error_mean,

      squared_error_median =
        squared_error_median,

      efficiency =
        relative_efficiency,

      bootstrap_means =
        bootstrap_means,

      bootstrap_medians =
        bootstrap_medians
    )
  )
}



# 2. FIXED-SIZE CONTAMINATED SAMPLE FUNCTION


generate_contaminated_sample <- function(
    n,
    contamination,
    clean_mean = 4,
    clean_sd = 2,
    outlier_mean = 15,
    outlier_sd = 2) {

  # Adding 0.5 and taking floor avoids R's
  # round-to-even behavior for values such as 1.5.
  n_outliers <- floor(
    n * contamination + 0.5
  )

  n_clean <- n - n_outliers

  clean_data <- rnorm(
    n_clean,
    mean = clean_mean,
    sd = clean_sd
  )

  if (n_outliers > 0) {

    outlier_data <- rnorm(
      n_outliers,
      mean = outlier_mean,
      sd = outlier_sd
    )

    x <- c(
      clean_data,
      outlier_data
    )

  } else {

    outlier_data <- numeric(0)
    x <- clean_data
  }

  realized_contamination <-
    n_outliers / n

  invisible(
    list(

      sample = x,

      clean_data =
        clean_data,

      outlier_data =
        outlier_data,

      n = n,

      n_clean =
        n_clean,

      n_outliers =
        n_outliers,

      requested_contamination =
        contamination,

      realized_contamination =
        realized_contamination
    )
  )
}



# 3. DETAILED CLEAN SAMPLE ANALYSIS


set.seed(123)

clean_information <- generate_contaminated_sample(
  n = 30,
  contamination = 0
)

x_clean <- clean_information$sample

clean_results <- bootest(
  x = x_clean,
  B = B_detailed,
  conf_level = 0.95,
  true_value = true_value,
  scenario_name = "Clean Sample",
  make_plots = TRUE,
  save_plot = TRUE
)



# 4. DETAILED CONTAMINATED SAMPLE ANALYSIS


set.seed(456)

contaminated_information <-
  generate_contaminated_sample(

    n = 30,

    contamination = 0.10,

    clean_mean = 4,
    clean_sd = 2,

    outlier_mean = 15,
    outlier_sd = 2
  )

x_contaminated <-
  contaminated_information$sample

contaminated_results <- bootest(

  x = x_contaminated,

  B = B_detailed,

  conf_level = 0.95,

  true_value = true_value,

  scenario_name =
    "Contaminated Sample",

  make_plots = TRUE,

  save_plot = TRUE
)



# 5. CLEAN VERSUS CONTAMINATED COMPARISON


sample_information_table <- data.frame(

  Scenario = c(
    "Clean",
    "Contaminated"
  ),

  Sample_Size = c(
    clean_information$n,
    contaminated_information$n
  ),

  Number_Clean = c(
    clean_information$n_clean,
    contaminated_information$n_clean
  ),

  Number_Outliers = c(
    clean_information$n_outliers,
    contaminated_information$n_outliers
  ),

  Requested_Contamination = c(
    clean_information$requested_contamination,
    contaminated_information$requested_contamination
  ),

  Realized_Contamination = c(
    clean_information$realized_contamination,
    contaminated_information$realized_contamination
  )
)

cat("\n====================================================\n")
cat("CLEAN AND CONTAMINATED SAMPLE INFORMATION\n")
cat("====================================================\n")

print(sample_information_table)


clean_summary <- data.frame(
  Scenario = "Clean",
  clean_results$summary
)

contaminated_summary <- data.frame(
  Scenario = "Contaminated",
  contaminated_results$summary
)

clean_contaminated_table <- rbind(
  clean_summary,
  contaminated_summary
)

cat("\n====================================================\n")
cat("DETAILED CLEAN VERSUS CONTAMINATED COMPARISON\n")
cat("====================================================\n")

print(clean_contaminated_table)


relative_efficiency_table <- data.frame(

  Scenario = c(
    "Clean",
    "Contaminated"
  ),

  Relative_Efficiency = c(
    clean_results$efficiency,
    contaminated_results$efficiency
  ),

  Mean_Variance = c(
    clean_results$var_mean,
    contaminated_results$var_mean
  ),

  Median_Variance = c(
    clean_results$var_median,
    contaminated_results$var_median
  ),

  Mean_Squared_Error_to_Target = c(
    clean_results$squared_error_mean,
    contaminated_results$squared_error_mean
  ),

  Median_Squared_Error_to_Target = c(
    clean_results$squared_error_median,
    contaminated_results$squared_error_median
  )
)

cat("\n====================================================\n")
cat("RELATIVE EFFICIENCY COMPARISON\n")
cat("====================================================\n")

print(relative_efficiency_table)


# Save tables
write.csv(
  sample_information_table,
  "results/sample_information.csv",
  row.names = FALSE
)

write.csv(
  clean_contaminated_table,
  "results/clean_contaminated_bootstrap_results.csv",
  row.names = FALSE
)

write.csv(
  relative_efficiency_table,
  "results/relative_efficiency_comparison.csv",
  row.names = FALSE
)


# Raw sample comparison plot
png(
  filename =
    "figures/clean_vs_contaminated_boxplot.png",
  width = 900,
  height = 600
)

boxplot(
  x_clean,
  x_contaminated,
  names = c(
    "Clean",
    "Contaminated"
  ),
  col = c(
    "lightblue",
    "lightpink"
  ),
  main =
    "Clean and Contaminated Samples",
  ylab = "Observed Value"
)

abline(
  h = true_value,
  col = "darkgreen",
  lwd = 2,
  lty = 2
)

dev.off()



# 6. MONTE CARLO ESTIMATOR COMPARISON FUNCTION


compare_estimators <- function(
    n,
    contamination,
    M = 1000,
    true_value = 4,
    clean_mean = 4,
    clean_sd = 2,
    outlier_mean = 15,
    outlier_sd = 2) {

  mean_estimates <- rep(0, M)
  median_estimates <- rep(0, M)

  n_outliers <- floor(
    n * contamination + 0.5
  )

  n_clean <- n - n_outliers

  realized_contamination <-
    n_outliers / n

  for (i in 1:M) {

    clean_data <- rnorm(
      n_clean,
      mean = clean_mean,
      sd = clean_sd
    )

    if (n_outliers > 0) {

      outlier_data <- rnorm(
        n_outliers,
        mean = outlier_mean,
        sd = outlier_sd
      )

      x <- c(
        clean_data,
        outlier_data
      )

    } else {

      x <- clean_data
    }

    mean_estimates[i] <- mean(x)
    median_estimates[i] <- median(x)
  }

  average_mean_estimate <-
    mean(mean_estimates)

  average_median_estimate <-
    mean(median_estimates)

  bias_mean <-
    average_mean_estimate - true_value

  bias_median <-
    average_median_estimate - true_value

  variance_mean <-
    var(mean_estimates)

  variance_median <-
    var(median_estimates)

  mse_mean <- mean(
    (mean_estimates - true_value)^2
  )

  mse_median <- mean(
    (median_estimates - true_value)^2
  )

  relative_efficiency <-
    variance_median / variance_mean

  if (variance_mean < variance_median) {

    best_by_variance <- "Mean"

  } else if (variance_median < variance_mean) {

    best_by_variance <- "Median"

  } else {

    best_by_variance <- "Equal"
  }

  if (mse_mean < mse_median) {

    best_by_mse <- "Mean"

  } else if (mse_median < mse_mean) {

    best_by_mse <- "Median"

  } else {

    best_by_mse <- "Equal"
  }

  result <- data.frame(

    Sample_Size = n,

    Requested_Contamination =
      contamination,

    Realized_Contamination =
      realized_contamination,

    Number_Clean =
      n_clean,

    Number_Outliers =
      n_outliers,

    Monte_Carlo_Repetitions =
      M,

    Average_Mean_Estimate =
      average_mean_estimate,

    Average_Median_Estimate =
      average_median_estimate,

    Bias_Mean =
      bias_mean,

    Bias_Median =
      bias_median,

    Variance_Mean =
      variance_mean,

    Variance_Median =
      variance_median,

    MSE_Mean =
      mse_mean,

    MSE_Median =
      mse_median,

    Relative_Efficiency =
      relative_efficiency,

    Best_by_Variance =
      best_by_variance,

    Best_by_MSE =
      best_by_mse
  )

  return(result)
}



# 7. CONTAMINATION SENSITIVITY ANALYSIS FOR n = 30


set.seed(789)

contamination_levels <- c(
  0,
  0.05,
  0.10,
  0.15,
  0.20
)

contamination_results <- data.frame()

for (j in 1:length(contamination_levels)) {

  current_result <- compare_estimators(

    n = 30,

    contamination =
      contamination_levels[j],

    M = M_simulation,

    true_value = true_value
  )

  contamination_results <- rbind(
    contamination_results,
    current_result
  )
}

cat("\n====================================================\n")
cat("CONTAMINATION SENSITIVITY RESULTS: n = 30\n")
cat("====================================================\n")

print(contamination_results)

write.csv(
  contamination_results,
  "results/contamination_sensitivity_n30.csv",
  row.names = FALSE
)



# 8. BIAS VERSUS CONTAMINATION


png(
  filename =
    "figures/bias_vs_contamination.png",
  width = 900,
  height = 600
)

plot(

  contamination_results$Realized_Contamination * 100,

  contamination_results$Bias_Mean,

  type = "b",
  pch = 19,
  col = "blue",

  ylim = range(
    contamination_results$Bias_Mean,
    contamination_results$Bias_Median
  ),

  main = "Bias versus Contamination",

  xlab = "Realized Contamination Percentage",

  ylab = "Estimated Bias"
)

lines(

  contamination_results$Realized_Contamination * 100,

  contamination_results$Bias_Median,

  type = "b",
  pch = 17,
  col = "red"
)

abline(
  h = 0,
  lty = 2
)

legend(

  "topleft",

  legend = c(
    "Mean",
    "Median"
  ),

  col = c(
    "blue",
    "red"
  ),

  pch = c(
    19,
    17
  ),

  lty = 1
)

dev.off()



# 9. MSE VERSUS CONTAMINATION


png(
  filename =
    "figures/mse_vs_contamination.png",
  width = 900,
  height = 600
)

plot(

  contamination_results$Realized_Contamination * 100,

  contamination_results$MSE_Mean,

  type = "b",
  pch = 19,
  col = "blue",

  ylim = range(
    contamination_results$MSE_Mean,
    contamination_results$MSE_Median
  ),

  main = "MSE versus Contamination",

  xlab = "Realized Contamination Percentage",

  ylab = "Mean Squared Error"
)

lines(

  contamination_results$Realized_Contamination * 100,

  contamination_results$MSE_Median,

  type = "b",
  pch = 17,
  col = "red"
)

legend(

  "topleft",

  legend = c(
    "Mean",
    "Median"
  ),

  col = c(
    "blue",
    "red"
  ),

  pch = c(
    19,
    17
  ),

  lty = 1
)

dev.off()



# 10. RELATIVE EFFICIENCY VERSUS CONTAMINATION


png(
  filename =
    "figures/relative_efficiency_vs_contamination.png",
  width = 900,
  height = 600
)

plot(

  contamination_results$Realized_Contamination * 100,

  contamination_results$Relative_Efficiency,

  type = "b",
  pch = 19,
  col = "darkgreen",

  main =
    "Relative Efficiency versus Contamination",

  xlab =
    "Realized Contamination Percentage",

  ylab =
    "Variance(Median) / Variance(Mean)"
)

abline(
  h = 1,
  col = "red",
  lwd = 2,
  lty = 2
)

dev.off()



# 11. MULTIPLE SAMPLE SIZES AND CONTAMINATION LEVELS


set.seed(2026)

sample_sizes <- c(
  30,
  50,
  100,
  200,
  500
)

contamination_levels <- c(
  0,
  0.05,
  0.10,
  0.15,
  0.20
)

all_simulation_results <- data.frame()

for (i in 1:length(sample_sizes)) {

  for (j in 1:length(contamination_levels)) {

    cat(
      "Running simulation for n =",
      sample_sizes[i],
      "and contamination =",
      contamination_levels[j],
      "\n"
    )

    current_result <- compare_estimators(

      n = sample_sizes[i],

      contamination =
        contamination_levels[j],

      M = M_simulation,

      true_value = true_value
    )

    all_simulation_results <- rbind(
      all_simulation_results,
      current_result
    )
  }
}

cat("\n====================================================\n")
cat("FULL SAMPLE-SIZE AND CONTAMINATION RESULTS\n")
cat("====================================================\n")

print(all_simulation_results)

write.csv(
  all_simulation_results,
  "results/full_simulation_results.csv",
  row.names = FALSE
)



# 12. MSE VERSUS SAMPLE SIZE


selected_contamination_levels <- c(
  0,
  0.10,
  0.20
)

png(
  filename =
    "figures/mse_vs_sample_size.png",
  width = 1500,
  height = 500
)

par(mfrow = c(1, 3))

for (k in 1:length(
  selected_contamination_levels
)) {

  selected_results <-
    all_simulation_results[

      abs(
        all_simulation_results$Requested_Contamination -
          selected_contamination_levels[k]
      ) < 0.000001,

    ]

  plot(

    selected_results$Sample_Size,

    selected_results$MSE_Mean,

    type = "b",
    pch = 19,
    col = "blue",

    ylim = range(
      selected_results$MSE_Mean,
      selected_results$MSE_Median
    ),

    main = paste0(
      selected_contamination_levels[k] * 100,
      "% Contamination"
    ),

    xlab = "Sample Size",

    ylab = "Mean Squared Error"
  )

  lines(

    selected_results$Sample_Size,

    selected_results$MSE_Median,

    type = "b",
    pch = 17,
    col = "red"
  )

  legend(

    "topright",

    legend = c(
      "Mean",
      "Median"
    ),

    col = c(
      "blue",
      "red"
    ),

    pch = c(
      19,
      17
    ),

    lty = 1,

    cex = 0.8
  )
}

par(mfrow = c(1, 1))

dev.off()



# 13. BOOTSTRAP CONFIDENCE INTERVAL FUNCTION


bootstrap_ci <- function(
    x,
    B = 500,
    conf_level = 0.95) {

  alpha <- 1 - conf_level
  n <- length(x)

  bootstrap_means <- rep(0, B)
  bootstrap_medians <- rep(0, B)

  for (i in 1:B) {

    samp <- sample(
      x,
      n,
      replace = TRUE
    )

    bootstrap_means[i] <- mean(samp)
    bootstrap_medians[i] <- median(samp)
  }

  lower_mean <- quantile(
    bootstrap_means,
    alpha / 2,
    na.rm = TRUE
  )[[1]]

  upper_mean <- quantile(
    bootstrap_means,
    1 - alpha / 2,
    na.rm = TRUE
  )[[1]]

  lower_median <- quantile(
    bootstrap_medians,
    alpha / 2,
    na.rm = TRUE
  )[[1]]

  upper_median <- quantile(
    bootstrap_medians,
    1 - alpha / 2,
    na.rm = TRUE
  )[[1]]

  invisible(
    list(

      ci_mean = c(
        lower_mean,
        upper_mean
      ),

      ci_median = c(
        lower_median,
        upper_median
      )
    )
  )
}



# 14. CONFIDENCE INTERVAL COVERAGE FUNCTION


estimate_bootstrap_coverage <- function(
    n,
    contamination,
    M = 300,
    B = 500,
    conf_level = 0.95,
    true_value = 4,
    clean_mean = 4,
    clean_sd = 2,
    outlier_mean = 15,
    outlier_sd = 2) {

  coverage_mean <- rep(0, M)
  coverage_median <- rep(0, M)

  width_mean <- rep(0, M)
  width_median <- rep(0, M)

  n_outliers <- floor(
    n * contamination + 0.5
  )

  n_clean <- n - n_outliers

  realized_contamination <-
    n_outliers / n

  for (i in 1:M) {

    clean_data <- rnorm(
      n_clean,
      mean = clean_mean,
      sd = clean_sd
    )

    if (n_outliers > 0) {

      outlier_data <- rnorm(
        n_outliers,
        mean = outlier_mean,
        sd = outlier_sd
      )

      x <- c(
        clean_data,
        outlier_data
      )

    } else {

      x <- clean_data
    }

    ci_results <- bootstrap_ci(
      x = x,
      B = B,
      conf_level = conf_level
    )

    if (
      ci_results$ci_mean[1] <= true_value &&
      ci_results$ci_mean[2] >= true_value
    ) {

      coverage_mean[i] <- 1
    }

    if (
      ci_results$ci_median[1] <= true_value &&
      ci_results$ci_median[2] >= true_value
    ) {

      coverage_median[i] <- 1
    }

    width_mean[i] <-
      ci_results$ci_mean[2] -
      ci_results$ci_mean[1]

    width_median[i] <-
      ci_results$ci_median[2] -
      ci_results$ci_median[1]
  }

  result <- data.frame(

    Sample_Size =
      n,

    Requested_Contamination =
      contamination,

    Realized_Contamination =
      realized_contamination,

    Monte_Carlo_Repetitions =
      M,

    Bootstrap_Repetitions =
      B,

    Coverage_Mean =
      mean(coverage_mean),

    Coverage_Median =
      mean(coverage_median),

    Average_Width_Mean =
      mean(width_mean),

    Average_Width_Median =
      mean(width_median)
  )

  return(result)
}



# 15. RUN SELECTED COVERAGE SCENARIOS


# This is the most computationally expensive section.
# The values of M_coverage and B_coverage may be increased
# after confirming that the complete script runs correctly.

set.seed(2027)

coverage_sample_sizes <- c(
  30,
  100
)

coverage_contamination_levels <- c(
  0,
  0.10,
  0.20
)

coverage_results <- data.frame()

for (i in 1:length(
  coverage_sample_sizes
)) {

  for (j in 1:length(
    coverage_contamination_levels
  )) {

    cat(
      "Running coverage simulation for n =",
      coverage_sample_sizes[i],
      "and contamination =",
      coverage_contamination_levels[j],
      "\n"
    )

    current_coverage <-
      estimate_bootstrap_coverage(

        n =
          coverage_sample_sizes[i],

        contamination =
          coverage_contamination_levels[j],

        M =
          M_coverage,

        B =
          B_coverage,

        conf_level =
          0.95,

        true_value =
          true_value
      )

    coverage_results <- rbind(
      coverage_results,
      current_coverage
    )
  }
}

cat("\n====================================================\n")
cat("BOOTSTRAP CONFIDENCE INTERVAL COVERAGE RESULTS\n")
cat("====================================================\n")

print(coverage_results)

write.csv(
  coverage_results,
  "results/bootstrap_coverage_results.csv",
  row.names = FALSE
)


# Coverage plot
coverage_matrix <- rbind(

  coverage_results$Coverage_Mean,

  coverage_results$Coverage_Median
)

coverage_names <- paste0(

  "n=",
  coverage_results$Sample_Size,

  "\n",

  round(
    coverage_results$Realized_Contamination * 100,
    1
  ),

  "%"
)

png(
  filename =
    "figures/bootstrap_coverage.png",
  width = 1100,
  height = 650
)

barplot(

  coverage_matrix,

  beside = TRUE,

  names.arg =
    coverage_names,

  col = c(
    "lightblue",
    "lightpink"
  ),

  ylim = c(
    0,
    1
  ),

  main =
    "Bootstrap Confidence Interval Coverage",

  xlab =
    "Sample Size and Contamination",

  ylab =
    "Estimated Coverage Probability"
)

abline(
  h = 0.95,
  col = "red",
  lwd = 2,
  lty = 2
)

legend(

  "bottomleft",

  legend = c(
    "Mean",
    "Median"
  ),

  fill = c(
    "lightblue",
    "lightpink"
  )
)

dev.off()



# 16. CUMULATIVE CONSISTENCY OF SAMPLE PROPORTION


consist <- function(
    N = 10000,
    p = 0.5,
    selected_n = c(
      10,
      50,
      100,
      500,
      1000,
      2000,
      5000,
      10000
    ),
    save_plot = TRUE) {

  # 1 represents a tail
  # 0 represents a head
  tosses <- rbinom(
    N,
    size = 1,
    prob = p
  )

  n <- 1:N

  cumulative_tails <-
    cumsum(tosses)

  sample_proportion <-
    cumulative_tails / n

  result <- data.frame(

    n = n,

    Cumulative_Tails =
      cumulative_tails,

    Sample_Proportion =
      sample_proportion
  )

  selected_n <-
    selected_n[selected_n <= N]

  selected_table <-
    result[selected_n, ]

  cat("\n====================================================\n")
  cat("SELECTED CUMULATIVE SAMPLE PROPORTIONS\n")
  cat("====================================================\n")

  print(selected_table)

  if (save_plot == TRUE) {

    png(
      filename =
        "figures/cumulative_sample_proportion.png",
      width = 1000,
      height = 600
    )
  }

  plot(

    n,

    sample_proportion,

    type = "l",

    ylim = c(
      0,
      1
    ),

    main =
      "Consistency of the Sample Proportion",

    ylab =
      "Cumulative Proportion of Tails",

    xlab =
      "Number of Coin Tosses"
  )

  abline(
    h = p,
    col = "red",
    lwd = 2
  )

  if (save_plot == TRUE) {
    dev.off()
  }

  invisible(
    list(

      full_results =
        result,

      selected_results =
        selected_table
    )
  )
}


set.seed(3030)

cumulative_results <- consist(
  N = 10000,
  p = 0.5,
  save_plot = TRUE
)

write.csv(

  cumulative_results$selected_results,

  "results/cumulative_proportion_selected_values.csv",

  row.names = FALSE
)



# 17. REPEATED CONSISTENCY SIMULATION


simulate_proportion_consistency <- function(
    sample_sizes,
    M = 5000,
    p = 0.5,
    tolerance = 0.05) {

  final_results <- data.frame()

  for (i in 1:length(sample_sizes)) {

    n <- sample_sizes[i]

    p_hat <- rep(0, M)

    for (j in 1:M) {

      p_hat[j] <- rbinom(
        1,
        size = n,
        prob = p
      ) / n
    }

    average_p_hat <-
      mean(p_hat)

    bias_p_hat <-
      average_p_hat - p

    variance_p_hat <-
      var(p_hat)

    theoretical_variance <-
      p * (1 - p) / n

    mse_p_hat <- mean(
      (p_hat - p)^2
    )

    probability_outside_tolerance <- mean(
      abs(p_hat - p) > tolerance
    )

    current_result <- data.frame(

      Sample_Size =
        n,

      Monte_Carlo_Repetitions =
        M,

      Average_p_hat =
        average_p_hat,

      Bias =
        bias_p_hat,

      Simulated_Variance =
        variance_p_hat,

      Theoretical_Variance =
        theoretical_variance,

      MSE =
        mse_p_hat,

      Probability_Outside_Tolerance =
        probability_outside_tolerance
    )

    final_results <- rbind(
      final_results,
      current_result
    )
  }

  return(final_results)
}


set.seed(4040)

proportion_sample_sizes <- c(
  10,
  50,
  100,
  500,
  1000,
  2000,
  5000,
  10000
)

proportion_results <-
  simulate_proportion_consistency(

    sample_sizes =
      proportion_sample_sizes,

    M = 5000,

    p = 0.5,

    tolerance = 0.05
  )

cat("\n====================================================\n")
cat("MONTE CARLO CONSISTENCY RESULTS\n")
cat("====================================================\n")

print(proportion_results)

write.csv(
  proportion_results,
  "results/proportion_consistency_results.csv",
  row.names = FALSE
)



# 18. AVERAGE PROPORTION VERSUS SAMPLE SIZE


png(
  filename =
    "figures/average_proportion_vs_sample_size.png",
  width = 900,
  height = 600
)

plot(

  proportion_results$Sample_Size,

  proportion_results$Average_p_hat,

  type = "b",

  pch = 19,

  log = "x",

  ylim = c(
    0.45,
    0.55
  ),

  main =
    "Average Sample Proportion versus Sample Size",

  xlab =
    "Sample Size (Log Scale)",

  ylab =
    "Average Sample Proportion"
)

abline(
  h = 0.5,
  col = "red",
  lwd = 2,
  lty = 2
)

dev.off()



# 19. SIMULATED AND THEORETICAL VARIANCE


png(
  filename =
    "figures/proportion_variance_vs_sample_size.png",
  width = 900,
  height = 600
)

plot(

  proportion_results$Sample_Size,

  proportion_results$Simulated_Variance,

  type = "b",

  pch = 19,

  col = "blue",

  log = "xy",

  ylim = range(
    proportion_results$Simulated_Variance,
    proportion_results$Theoretical_Variance
  ),

  main =
    "Variance of the Sample Proportion",

  xlab =
    "Sample Size (Log Scale)",

  ylab =
    "Variance (Log Scale)"
)

lines(

  proportion_results$Sample_Size,

  proportion_results$Theoretical_Variance,

  type = "b",

  pch = 17,

  col = "red"
)

legend(

  "topright",

  legend = c(
    "Simulated Variance",
    "Theoretical Variance"
  ),

  col = c(
    "blue",
    "red"
  ),

  pch = c(
    19,
    17
  ),

  lty = 1
)

dev.off()



# 20. PROBABILITY OF LARGE ESTIMATION ERROR


png(
  filename =
    "figures/probability_outside_tolerance.png",
  width = 900,
  height = 600
)

plot(

  proportion_results$Sample_Size,

  proportion_results$Probability_Outside_Tolerance,

  type = "b",

  pch = 19,

  log = "x",

  main =
    "Probability of Large Estimation Error",

  xlab =
    "Sample Size (Log Scale)",

  ylab =
    "P(|p-hat - 0.5| > 0.05)"
)

dev.off()



# 21. BOXPLOTS OF REPEATED SAMPLE PROPORTIONS


create_proportion_boxplots <- function(
    sample_sizes,
    M = 2000,
    p = 0.5,
    save_plot = TRUE) {

  proportion_list <- vector(
    mode = "list",
    length = length(sample_sizes)
  )

  for (i in 1:length(sample_sizes)) {

    n <- sample_sizes[i]

    p_hat <- rep(0, M)

    for (j in 1:M) {

      p_hat[j] <- rbinom(
        1,
        size = n,
        prob = p
      ) / n
    }

    proportion_list[[i]] <- p_hat
  }

  names(proportion_list) <-
    sample_sizes

  if (save_plot == TRUE) {

    png(
      filename =
        "figures/sample_proportion_boxplots.png",
      width = 1100,
      height = 650
    )
  }

  boxplot(

    proportion_list,

    main =
      "Distribution of Sample Proportions",

    xlab =
      "Sample Size",

    ylab =
      "Sample Proportion of Tails",

    col =
      "lightblue",

    outline =
      FALSE
  )

  abline(
    h = p,
    col = "red",
    lwd = 2,
    lty = 2
  )

  if (save_plot == TRUE) {
    dev.off()
  }

  invisible(proportion_list)
}


set.seed(5050)

proportion_boxplot_results <-
  create_proportion_boxplots(

    sample_sizes =
      proportion_sample_sizes,

    M = 2000,

    p = 0.5,

    save_plot = TRUE
  )



# 22. SAVE REPRODUCIBILITY INFORMATION


capture.output(
  sessionInfo(),
  file = "results/session_info.txt"
)



# 23. FINAL MESSAGE


cat("\n====================================================\n")
cat("PROJECT ANALYSIS COMPLETED\n")
cat("====================================================\n")

cat(
  "Tables were saved in the results folder.\n"
)

cat(
  "Figures were saved in the figures folder.\n"
)

cat(
  "Session information was saved in results/session_info.txt.\n"
)