# Author: Silvia Ianeselli
# Date: 24/11/2025
# Scope: This script uses data_transformed created in 01_Data_Transformation.R and cleans it by 
  # 1. keeping only the data range of interest (1960-01-01 - 2024-12-31)
  # 2. Investigating the NAs in the data and dealing with them

rm(list = ls())

# load data
data_transformed <- readRDS("02_Input/data_transformed.rds")

# data_transformed has 800 obs and 127 variables

# Keep data from January 1960 to Dec 2024 only - 780 observations
data_cleaned_withNA <- data_transformed[
  data_transformed$date >= as.Date("1960-01-01") &
    data_transformed$date <= as.Date("2024-12-31"),
]
# Data now has 780 obs and 127 variables

# Investigate columns with NAs to decide how to deal with them

# Count NAs per variable
na_counts <- sapply(data_cleaned_withNA, function(x) sum(is.na(x)))

# Keep only variables with >0 NAs and order by decreasing NA count
vars_to_check <- names(sort(na_counts[na_counts > 0], decreasing = TRUE))
print(vars_to_check)


# Initialize list to store results
na_summary_list <- list()

for (varname in vars_to_check) {
  
  # Subset rows where variable is NA
  na_rows <- data_cleaned_withNA[is.na(data_cleaned_withNA[[varname]]), ]
  
  # Min and max date for missing values
  min_date <- if(nrow(na_rows) > 0) min(na_rows$date, na.rm = TRUE) else NA
  max_date <- if(nrow(na_rows) > 0) max(na_rows$date, na.rm = TRUE) else NA
  
  # Count missing values per year
  missing_years <- format(na_rows$date, "%Y")
  year_table <- table(missing_years)
  
  # Store in list
  na_summary_list[[varname]] <- list(
    min_date = min_date,
    max_date = max_date,
    missing_per_year = year_table
  )
  
}

# View results
na_summary_list

# Although the original paper we are replicating drops all the variables with NAs,
# we will input the NAs using the PCA method normally applies to Fred data
# method is explained here "https://s3.amazonaws.com/real.stlouisfed.org/wp/2015/2015-012.pdf"

# Prepare pca function for imputting missing values

pca_impute <- function(X, ncomp = 3, maxiter = 100, tol = 1e-6, verbose = TRUE) {
  # --------------------------------------------------------------
  # PCA-based EM imputation following Stock & Watson (2002)
  # --------------------------------------------------------------
  # X       : data matrix (rows = observations, cols = variables) with NAs
  # ncomp   : number of principal components (factors)
  # maxiter : maximum iterations
  # tol     : convergence tolerance
  # verbose : print iteration info
  # --------------------------------------------------------------
  
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  
  # Identify missing entries
  missing <- is.na(X)
  
  # If no missing values, return original
  if (!any(missing)) {
    if (verbose) cat("No missing values detected.\n")
    return(X)
  }
  
  # Step 1: Calculate column means and SDs from observed data only
  # These stay FIXED throughout
  col_mean <- numeric(p)
  col_sd <- numeric(p)
  
  for (j in 1:p) {
    obs_vals <- X[!missing[, j], j]
    if (length(obs_vals) > 0) {
      col_mean[j] <- mean(obs_vals)
      col_sd[j] <- sd(obs_vals)
      if (is.na(col_sd[j]) || col_sd[j] == 0) {
        col_sd[j] <- 1
      }
    } else {
      col_mean[j] <- 0
      col_sd[j] <- 1
    }
  }
  
  # Step 2: Standardize the data
  X_std <- X
  for (j in 1:p) {
    X_std[, j] <- (X[, j] - col_mean[j]) / col_sd[j]
  }
  
  # Step 3: Initialize missing values to 0 (mean of standardized data)
  X_std[missing] <- 0
  
  # EM Algorithm
  converged <- FALSE
  iter <- 0
  X_filled_old <- X_std
  
  while (!converged && iter < maxiter) {
    iter <- iter + 1
    
    # E-step: Perform PCA on current filled matrix
    # Center the data (should already be centered, but for safety)
    X_centered <- scale(X_std, center = TRUE, scale = FALSE)
    
    # SVD
    svd_result <- svd(X_centered)
    
    # Extract first ncomp components
    U <- svd_result$u[, 1:ncomp, drop = FALSE]
    D <- svd_result$d[1:ncomp]
    V <- svd_result$v[, 1:ncomp, drop = FALSE]
    
    # M-step: Reconstruct data using first ncomp components
    X_reconstructed <- U %*% diag(D, ncomp, ncomp) %*% t(V)
    
    # Add back the center (which should be ~0 for standardized data)
    col_centers <- attr(X_centered, "scaled:center")
    X_reconstructed <- sweep(X_reconstructed, 2, col_centers, "+")
    
    # Update only the missing values
    X_std_new <- X_std
    X_std_new[missing] <- X_reconstructed[missing]
    
    # Check convergence
    diff <- sqrt(sum((X_std_new - X_filled_old)^2)) / sqrt(sum(X_filled_old^2) + 1e-10)
    
    if (verbose) {
      cat(sprintf("Iteration %d: relative change = %.6f\n", iter, diff))
    }
    
    if (diff < tol) {
      converged <- TRUE
    }
    
    X_filled_old <- X_std_new
    X_std <- X_std_new
  }
  
  if (verbose) {
    if (converged) {
      cat(sprintf("Converged after %d iterations.\n", iter))
    } else {
      cat(sprintf("Did not converge after %d iterations.\n", maxiter))
    }
  }
  
  # Step 4: Transform back to original scale
  X_imputed <- X_std
  for (j in 1:p) {
    X_imputed[, j] <- X_std[, j] * col_sd[j] + col_mean[j]
  }
  
  return(X_imputed)
}



# Use ALL numeric columns (excluding date) for imputation
numeric_cols <- names(data_cleaned_withNA)[sapply(data_cleaned_withNA, is.numeric)]
# Remove 'date' if it's in numeric_cols
numeric_cols <- setdiff(numeric_cols, "date")

# Extract ALL numeric columns
X_for_impute <- as.matrix(data_cleaned_withNA[, numeric_cols])

# Run the imputation using ALL columns (using 5 factors)
X_completed <- pca_impute(X_for_impute, ncomp = 5, verbose = TRUE)

# Put the completed data back
data_cleaned <- data_cleaned_withNA
data_cleaned[, numeric_cols] <- X_completed

# Verify no more NAs
cat("\nFinal check - columns with NAs:", 
    sum(colSums(is.na(data_cleaned)) > 0), "\n")

# Compare means before and after (only for columns that had NAs)
na_cols <- names(which(sapply(data_cleaned_withNA, function(x) any(is.na(x)))))

cat("\n=== Comparison of means (variables with NAs) ===\n")
cat("\nBefore imputation:\n")
for (col in na_cols) {
  mean_val <- mean(data_cleaned_withNA[[col]], na.rm = TRUE)
  cat(sprintf("%s: %.5f\n", col, mean_val))
}

cat("\nAfter imputation:\n")
for (col in na_cols) {
  cat(sprintf("%s: %.5f\n", col, mean(data_cleaned[[col]])))
}

# Multiple CPIAUCSL by 100 to easily read it as inflation (After log transformation 5)
data_cleaned$CPIAUCSL <- data_cleaned$CPIAUCSL*100

# save dataset
saveRDS(data_cleaned, file = "02_Input/data_cleaned.rds")
