get_pred_dist <- function(obs, prob, xlab = "Predicted Probability") {
  plot_df <- data.table::data.table(
    "Prob" = prob,
    "Outcome" = factor(ifelse(obs == 1, "Event", "No Event"))
  )
  PR_dist <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$Outcome,
      y = .data$Prob
    )
  ) +
    ggplot2::geom_violin(
      position = ggplot2::position_dodge(width = .75),
      linewidth = 1
    ) +
    ggplot2::geom_boxplot(
      width = 0.1,
      outlier.shape = NA
    ) +
    ggplot2::ylab(xlab) +
    ggplot2::theme_bw(base_size = 12)

  PR_dist
}


#' Generate a Calibration Plot with Distribution Histogram
#'
#' @description
#' Creates a two-panel calibration plot for binary outcome prediction models.
#' The top panel displays the empirical calibration curve (fitted via GAM or LOWESS)
#' along with binned mean predicted versus observed probabilities. The bottom panel
#' shows the distribution of predicted probabilities as a histogram (count or density).
#'
#' @param data A data frame or \code{data.table} containing observed binary outcomes
#'   and predicted probabilities.
#' @param outcome_col A character string specifying the column name for observed binary
#'   outcomes (0 or 1). Default is \code{"observed"}.
#' @param prob_col A character string specifying the column name for predicted
#'   probabilities (ranging from 0 to 1). Default is \code{"probability"}.
#' @param n_bins An integer specifying the number of quantile-based bins for grouping
#'   predicted probabilities. Default is \code{10}.
#' @param plot_title A character string specifying the title of the calibration plot.
#'   Default is \code{"Calibration Plot"}.
#' @param hist_bins An integer specifying the number of bins for the bottom histogram.
#'   Default is \code{100}.
#' @param hist_type A character string specifying the type of histogram in the bottom
#'   panel: \code{"count"} or \code{"density"}. Default is \code{"count"}.
#' @param smooth_method A character string specifying the smoothing algorithm for the
#'   calibration curve: \code{"gam"} (Generalized Additive Model) or \code{"lowess"}
#'   (Locally Weighted Scatterplot Smoothing). Default is \code{"gam"}.
#' @param lowess_frac A numeric value specifying the smoothing span (\code{f}) for the
#'   LOWESS smoother. Only used when \code{smooth_method = "lowess"}. Default is \code{0.5}.
#' @param lowess_delta A numeric value specifying the distance threshold (\code{delta})
#'   for speed up in LOWESS calculation. Only used when \code{smooth_method = "lowess"}.
#'   Default is \code{0.01}.
#' @param axis_lim A numeric vector of length 1 or 2 specifying the limits for both
#'   X and Y axes (e.g., \code{c(0, 1)} or \code{0.5}). Default is \code{c(0, 1)}.
#'
#' @return A combined \code{patchwork} object containing the top calibration plot
#'   and bottom probability distribution histogram.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' set.seed(123)
#' sample_data <- data.table(
#'   observed = rbinom(1000, 1, 0.2),
#'   probability = runif(1000, 0, 0.5)
#' )
#'
#' # Basic usage with default parameters
#' draw_calibration_plot(sample_data)
#'
#' # Customised axes and LOWESS smoother
#' draw_calibration_plot(
#'   data = sample_data,
#'   smooth_method = "lowess",
#'   axis_lim = 0.6
#' )
#' }
cali_plot.logistic <- function(
  data,
  outcome_col = "observed",
  prob_col = "probability",
  n_bins = 10,
  plot_title = "Calibration Plot",
  hist_bins = 100,
  hist_type = c("density", "count"),
  smooth_method = c("lowess", "gam"), # Choice of smoother
  lowess_frac = 0.5, # Window size for lowess
  lowess_delta = 0.01, # Distance threshold for lowess
  axis_lim = c(0, 1) # Axis limits applied to both X and Y axes
) {
  # Match arguments to ensure valid input
  hist_type <- match.arg(hist_type)
  smooth_method <- match.arg(smooth_method)

  # ---- Format axis_lim parameter
  if (length(axis_lim) == 1) {
    axis_lim <- c(0, axis_lim)
  }

  # Dynamically compute clean breaks based on axis_lim range
  axis_breaks <- pretty(axis_lim, n = 5)

  # ---- Data preparation using data.table
  dt <- data.table::as.data.table(data)
  plot_dt <- data.table::data.table(
    outcome = dt[[outcome_col]],
    prob = dt[[prob_col]]
  )

  # ---- Calculate binned data (pure data.table approach)
  # Extract quantiles and remove duplicate thresholds to prevent binning errors
  q_breaks <- unique(
    quantile(
      plot_dt$prob,
      probs = seq(0, 1, length.out = n_bins + 1),
      na.rm = TRUE
    )
  )

  # If all predicted probabilities are identical or unique breaks are fewer than 2, skip binning safely
  if (length(q_breaks) > 1) {
    plot_dt[,
      bin := cut(
        prob,
        breaks = q_breaks,
        include.lowest = TRUE,
        labels = FALSE
      )
    ]

    # Aggregate mean values per bin
    binned_df <- plot_dt[
      !is.na(bin),
      .(
        mean_pred = mean(prob, na.rm = TRUE),
        mean_obs = mean(outcome, na.rm = TRUE)
      ),
      by = bin
    ]
  } else {
    # Fallback for single unique probability value across all observations
    binned_df <- plot_dt[,
      .(
        mean_pred = mean(prob, na.rm = TRUE),
        mean_obs = mean(outcome, na.rm = TRUE)
      )
    ]
  }

  # ---- Draw top main plot (Calibration Curve)
  p_top <- ggplot2::ggplot() +
    # Ideal diagonal line
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "gray",
      linewidth = 0.8
    )

  # Smoothed curve
  if (smooth_method == "gam") {
    p_top <- p_top +
      ggplot2::geom_smooth(
        data = plot_dt,
        ggplot2::aes(x = prob, y = outcome),
        method = "gam",
        formula = y ~ s(x, bs = "cs"),
        color = "black",
        se = FALSE,
        linewidth = 0.8
      )
  } else if (smooth_method == "lowess") {
    fit_lowess <- stats::lowess(
      x = plot_dt$prob,
      y = plot_dt$outcome,
      f = lowess_frac,
      delta = lowess_delta,
      iter = 0
    )
    lowess_df <- data.frame(prob = fit_lowess$x, outcome = fit_lowess$y)

    p_top <- p_top +
      ggplot2::geom_line(
        data = lowess_df,
        ggplot2::aes(x = prob, y = outcome),
        color = "black",
        linewidth = 0.8
      )
  }

  p_top <- p_top +
    # Binned scatter points
    ggplot2::geom_point(
      data = binned_df,
      ggplot2::aes(x = mean_pred, y = mean_obs),
      shape = 21,
      fill = "white",
      color = "black",
      size = 5,
      alpha = 0.9
    ) +
    # Set coordinate axes with dynamic breaks
    ggplot2::scale_x_continuous(breaks = axis_breaks) +
    ggplot2::scale_y_continuous(breaks = axis_breaks) +
    ggplot2::coord_cartesian(xlim = axis_lim, ylim = axis_lim) +
    ggplot2::labs(title = plot_title, y = "Observed Frequency") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    )

  # ---- Draw bottom histogram
  p_bottom <- ggplot2::ggplot(plot_dt, ggplot2::aes(x = prob))

  if (hist_type == "density") {
    p_bottom <- p_bottom +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        bins = hist_bins,
        fill = "gray",
        color = "black",
        linewidth = 0.1
      ) +
      ggplot2::labs(x = "Predicted Probability", y = "Density")
  } else {
    p_bottom <- p_bottom +
      ggplot2::geom_histogram(
        bins = hist_bins,
        fill = "gray",
        color = "black",
        linewidth = 0.1
      ) +
      ggplot2::labs(x = "Predicted Probability", y = "Count")
  }

  p_bottom <- p_bottom +
    ggplot2::scale_x_continuous(breaks = axis_breaks) +
    ggplot2::coord_cartesian(xlim = axis_lim) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # ---- Combine plots using patchwork
  combined_plot <- patchwork::wrap_plots(
    p_top,
    p_bottom,
    ncol = 1,
    heights = c(4, 1)
  )

  # Return the combined plot
  return(combined_plot)
}


#' @title Calibration Plot for Linear Regression
#' @description Generates a scatter plot of predicted versus observed continuous values with a smoothed trend line.
#' @param data A data.table or data.frame containing the predictions and outcomes.
#' @param outcome_col A character string specifying the true continuous outcome column.
#' @param pred_col A character string specifying the predicted continuous value column.
#' @param sample_proportion Numeric between 0 and 1 indicating the percentage of data to sample (default: 1).
#' @param ... Additional arguments.
#' @import ggplot2
#' @import data.table
#' @export
cali_plot.linear <- function(
  data,
  outcome_col,
  pred_col,
  sample_proportion = 1,
  ...
) {
  # Convert to data.table to ensure consistent behaviour
  dt <- as.data.table(data)

  # Validate column existence
  if (!all(c(outcome_col, pred_col) %in% names(dt))) {
    stop(
      "The specified outcome or prediction columns are not found in the dataset."
    )
  }

  # Validate sample proportion input
  if (sample_proportion <= 0 || sample_proportion > 1) {
    stop(
      "The 'sample_proportion' must be greater than 0 and less than or equal to 1."
    )
  }

  # Randomly downsample the dataset if a proportion less than 1 is specified to minimise overplotting
  if (sample_proportion < 1) {
    n_sample <- round(nrow(dt) * sample_proportion)
    dt <- dt[sample(.N, n_sample)]
  }

  # Determine axis limits to ensure the reference line is correctly scaled
  min_val <- min(c(dt[[outcome_col]], dt[[pred_col]]), na.rm = TRUE)
  max_val <- max(c(dt[[outcome_col]], dt[[pred_col]]), na.rm = TRUE)

  # Construct the scatter plot mirroring standard aesthetic styles
  p <- ggplot(dt, aes(x = .data[[pred_col]], y = .data[[outcome_col]])) +
    geom_point(colour = "black", alpha = 0.3, size = 1.5) +
    # Add the smoothed trend line (red) and confidence interval (grey shaded area)
    geom_smooth(
      colour = "red",
      fill = "grey50",
      alpha = 0.4,
      method = "auto",
      se = TRUE
    ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      colour = "black"
    ) +
    scale_x_continuous(limits = c(min_val, max_val)) +
    scale_y_continuous(limits = c(min_val, max_val)) +
    labs(
      x = "Predicted Value",
      y = "Observed Value",
      title = "Calibration Plot (Linear)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0, face = "bold"),
      axis.line = element_line(colour = "black"),
      panel.grid.minor = element_blank()
    )

  return(p)
}
