#' Check and convert data to data.table
#' @param data A data.frame or data.table
#' @return A data.table
#'
#' @export
to_data_table <- function(data) {
  if (data.table::is.data.table(data)) {
    return(data)
  }
  return(data.table::as.data.table(data))
}

#' Convert factor columns to dummy variables
#' This function takes a data.table and a vector of variable names, and converts
#' the factor columns to dummy variables.
#' @param data A data.table containing the dataset.
#' @param vars A character vector of variable names to be converted to dummy variables.
#' @return A data.table with the specified factor columns converted to dummy variables.
#'
#' @export
dummy_vars <- function(data, vars) {
  # Copy the data to avoid modifying the original data.table by reference
  new_data <- copy(data)
  # Check and convert to data.table if necessary
  dt <- to_data_table(new_data)

  # Check if all specified variables exist in the dataset
  missing_vars <- setdiff(vars, names(dt))
  if (length(missing_vars) > 0) {
    stop(
      "The following variables are missing in the data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Check if the specified variables are either factor or character types
  # We use .SDcols to subset the data.table for checking
  valid_types <- sapply(dt[, .SD, .SDcols = vars], function(x) {
    is.factor(x) || is.character(x)
  })
  invalid_vars <- vars[!valid_types]

  if (length(invalid_vars) > 0) {
    stop(
      "The following variables must be factor or character types: ",
      paste(invalid_vars, collapse = ", ")
    )
  }

  # Loop through the specified variables to create dummy columns
  for (j in vars) {
    # Coerce character to factor to ensure stable behaviour in model.matrix
    if (is.character(dt[[j]])) {
      set(dt, j = j, value = as.factor(dt[[j]]))
    }

    dummy_mat <- stats::model.matrix.lm(
      ~ -1 + dt[[j]],
      na.action = "na.pass"
    )

    # Standardise the column names to format "OriginalVarName_CategoryName"
    colnames(dummy_mat) <- paste(
      j,
      sub("dt\\[\\[j\\]\\]", "", colnames(dummy_mat)),
      sep = "_"
    )

    # Attach the new dummy columns to the data.table by reference
    for (col_name in colnames(dummy_mat)) {
      set(dt, j = col_name, value = dummy_mat[, col_name])
    }
  }

  # Remove the original factor/character columns
  dt[, (vars) := NULL]

  return(dt)
}


#' Calculate the linear predictor for linear-based models
#'
#' An internal helper function that ensures input data is handled correctly,
#' checks for missing predictor columns, extracts the design matrix, and
#' calculates the linear combination ($X\beta + \beta_0$).
#'
#' @param object An object of class 'logistic' created by `build_model`.
#' @param new_data A `data.frame` or `data.table` containing new predictor values.
#' @return A numeric vector representing the calculated linear predictor values.
#' @keywords internal
calculate_lp <- function(object, new_data) {
  new_data <- to_data_table(new_data)

  coefs <- object$coefs
  coef_names <- names(coefs)

  # Separate Intercept and predictor coefficients
  intercept_val <- coefs[["(Intercept)"]]
  predictor_coefs <- coefs[coef_names != "(Intercept)"]
  predictor_names <- names(predictor_coefs)

  # Check if all required predictor columns exist in new_data
  missing_cols <- setdiff(predictor_names, names(new_data))
  if (length(missing_cols) > 0) {
    stop(
      "The following predictor variables are missing in `new_data`: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Extract design matrix X for predictors
  X <- as.matrix(new_data[, predictor_names, drop = FALSE, with = FALSE])

  # Calculate linear combination
  lp <- as.vector(X %*% predictor_coefs) + intercept_val
  return(lp)
}


#' Extract essential variables only model needs
#'
#' This function extracts only the variables that are necessary for the model,
#' based on the model equation and the outcome variable.
#'
#' @param data A data.table containing the dataset.
#' @param model_eq A named vector of coefficients from the model equation.
#' @param outcome A string representing the name of the outcome variable.
#' @param check A logical value indicating whether to check for missing variables in the dataset.
#' @param other_cols An optional character vector of additional columns to include in the output.
#' @return A data.table containing only the essential variables required for the model.
#'
#' @export
extract_variables <- function(
  data,
  model_obj,
  outcome = NULL,
  check = TRUE,
  other_cols = NULL
) {
  # Extract variable names from the model equation
  model_vars <- names(model_obj$coefs)

  # Remove Intercept
  model_vars_clean <- setdiff(model_vars, c("Intercept", "(Intercept)"))

  # Combine all required columns, ensuring uniqueness
  final_cols <- unique(c(outcome, "pregid", model_vars_clean, other_cols))

  # Check if all required variables exist in the data
  if (check) {
    missing_cols <- setdiff(final_cols, names(data))

    if (length(missing_cols) > 0) {
      stop(
        "The following required variables are missing from the dataset: ",
        paste(missing_cols, collapse = ", ")
      )
    }
  } else {
    # If check is FALSE, only extract columns that actually exist to prevent data.table errors
    final_cols <- intersect(final_cols, names(data))
  }

  # Subset the data.table using .SDcols
  # Note: This assumes 'data' is already a data.table object
  data <- data[, .SD, .SDcols = final_cols]

  return(data)
}
