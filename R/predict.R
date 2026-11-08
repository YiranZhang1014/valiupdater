#' Predict method for linear regression models
#'
#' Computes predicted response values for new data using a fitted linear regression model.
#'
#' @param object An object of class 'logistic' created by `build_model`.
#' @param new_data A `data.frame` or `data.table` containing the new predictor variables.
#' @param type A character string specifying the type of prediction required.
#'   Currently supports `"response"`.
#' @param ... Additional arguments passed to or from other methods.
#' @return A list containing the predicted response values (`$lp`).
#' @export
predict.linear <- function(
  object,
  new_data,
  type = c("response"),
  ...
) {
  type <- match.arg(type)

  # Utilize the core linear predictor function
  lp <- calculate_lp(object, new_data)

  res_lst <- list()
  if (type == "response") {
    res_lst$lp <- lp
  }

  return(res_lst)
}

#' Predict method for linear regression models
#'
#' Computes predicted response/probability values for new data using logistic model.
#'
#' @param object An object of class 'logistic' created by `build_model`.
#' @param new_data A `data.frame` or `data.table` containing the new predictor variables.
#' @param type A character string specifying the output type. Options are `"all"` (default),
#'   `"prob"` (predicted probabilities), or `"lp"` (linear predictors).
#' @param ... Additional arguments passed to or from other methods.
#' @return A list containing the requested prediction components (`$lp` and/or `$prob`).
#' @export
predict.logistic <- function(
  object,
  new_data,
  type = c("all", "link", "response"),
  ...
) {
  type <- match.arg(type)

  # Utilize the core linear predictor function
  linear_predictor <- calculate_lp(object, new_data)

  res_lst <- list()

  # Linear predictor
  if (type %in% c("link", "all")) {
    res_lst$linear_predictor <- linear_predictor
  }

  # Probability
  if (type %in% c("response", "all")) {
    res_lst$probability <- 1 / (1 + exp(-linear_predictor))
  }

  return(res_lst)
}
