#' Plot SWOT-adapted hydrological regime indicators
#'
#' @description Visualizes the four tables returned by [swot_regime()].
#'   All statistics represent sampled SWOT observations rather than continuous
#'   flow records. Plots return ggplot objects and can be customized with ggplot2.
#' @param regime A list returned by `swot_regime()` with `regime`, `annual`,
#'   `relative`, and `seasonal` tables.
#' @param plot One of `"seasonal"`, `"annual"`, `"relative"`, or `"coverage"`.
#' @param id Optional one or more feature IDs to plot. IDs are compared as
#'   character strings, preserving large SWORD identifiers.
#' @param facet If TRUE, facet by feature ID; if FALSE, overlay features.
#'   Defaults to TRUE when more than one feature is present.
#' @param show_n Show sample sizes in seasonal plots as point sizes.
#'   Seasonal subtitles also list years contributing valid observations,
#'   derived from the annual regime table.
#' @param relative_metric For `plot = "relative"`, one of `"median_anomaly"`,
#'   `"mean_anomaly"`, `"standardized_mean_anomaly"`, `"high_fraction"`,
#'   or `"low_fraction"`.
#' @param coverage_by For `plot = "coverage"`, use `"year"` or `"month"`.
#'   Monthly counts are aggregated over all years in the input record.
#' @param min_n Optional minimum number of valid observations for plotted
#'   seasonal points. Does not recalculate statistics suppressed by swot_regime().
#' @return A ggplot object.
#' @export
#' @examples
#' \dontrun{
#' result <- swot_regime(swot_data, input = "discharge")
#' swot_plot_regime(result)
#' swot_plot_regime(result, plot = "annual")
#' swot_plot_regime(result, plot = "relative", relative_metric = "high_fraction")
#' swot_plot_regime(result, plot = "coverage", coverage_by = "year")
#' }
swot_plot_regime <- function(regime,
                             plot = c("seasonal", "annual", "relative", "coverage"),
                             id = NULL,
                             facet = NULL,
                             show_n = TRUE,
                             relative_metric = c("median_anomaly", "mean_anomaly",
                                                 "standardized_mean_anomaly",
                                                 "high_fraction", "low_fraction"),
                             coverage_by = c("year", "month"),
                             min_n = NULL) {
  plot <- match.arg(plot)
  relative_metric <- match.arg(relative_metric)
  coverage_by <- match.arg(coverage_by)
  needed <- c("regime", "annual", "relative", "seasonal")
  if (!is.list(regime) || !all(needed %in% names(regime))) {
    stop("'regime' must be the list returned by swot_regime().", call. = FALSE)
  }
  if (!is.null(min_n) && (length(min_n) != 1L || !is.numeric(min_n) ||
                          is.na(min_n) || min_n < 1)) {
    stop("'min_n' must be NULL or a positive number.", call. = FALSE)
  }
  tab <- if (plot == "coverage") {
    regime[[if (coverage_by == "year") "annual" else "seasonal"]]
  } else {
    regime[[plot]]
  }
  if (!is.data.frame(tab) || nrow(tab) == 0L) {
    stop("The requested regime table contains no rows.", call. = FALSE)
  }
  # swot_regime() renames group_id to the source feature column when available.
  id_candidates <- c("group_id", "reach_id", "node_id")
  id_col <- intersect(id_candidates, names(tab))
  if (!length(id_col)) {
    stop("No feature identifier column found in the regime table.", call. = FALSE)
  }
  id_col <- id_col[1L]
  tab$.feature <- as.character(tab[[id_col]])
  if (!is.null(id)) {
    tab <- tab[tab$.feature %in% as.character(id), , drop = FALSE]
    if (nrow(tab) == 0L) stop("No observations for the requested 'id'.", call. = FALSE)
  }
  n_features <- length(unique(tab$.feature))
  if (is.null(facet)) facet <- n_features > 1L
  if (!is.logical(facet) || length(facet) != 1L || is.na(facet)) {
    stop("'facet' must be TRUE or FALSE.", call. = FALSE)
  }
  variable_type <- unique(as.character(tab$input))
  variable_type <- variable_type[!is.na(variable_type)]
  if (length(variable_type) != 1L) {
    stop("Plot one input type at a time (WSE or discharge).", call. = FALSE)
  }
  units <- if (variable_type == "wse") "WSE (m)" else "Discharge (m³/s)"
  month_labels <- month.abb
  gg <- ggplot2::ggplot
  aes <- ggplot2::aes
  if (plot == "seasonal") {
    # Determine contributing years from annual summaries. The seasonal table
    # contains n_years, but not the actual calendar years. Use the annual
    # table, respecting the feature IDs selected above.
    annual_years <- regime$annual
    year_subtitle <- "Median and observed P10–P90 range"
    if (is.data.frame(annual_years) && nrow(annual_years) > 0L &&
        all(c(id_col, "year", "n_valid") %in% names(annual_years))) {
      annual_years <- annual_years[
        as.character(annual_years[[id_col]]) %in% unique(tab$.feature) &
          !is.na(annual_years$n_valid) & annual_years$n_valid > 0 &
          !is.na(annual_years$year), , drop = FALSE
      ]
      years <- sort(unique(as.integer(annual_years$year)))
      if (length(years) > 0L) {
        year_subtitle <- paste0(year_subtitle, " | Years: ",
                                paste(years, collapse = ", "))
      }
    }
    required <- c("month", "median", "p10", "p90", "n_valid")
    if (!all(required %in% names(tab))) stop("Missing seasonal columns.", call. = FALSE)
    tab <- tab[!is.na(tab$month) & tab$month %in% 1:12, , drop = FALSE]
    if (!is.null(min_n)) tab <- tab[tab$n_valid >= min_n, , drop = FALSE]
    p <- gg(tab, aes(x = .data$month, y = .data$median,
                     group = .data$.feature, colour = .data$.feature)) +
      ggplot2::geom_ribbon(aes(ymin = .data$p10, ymax = .data$p90,
                               fill = .data$.feature),
                           alpha = 0.16, colour = NA, na.rm = TRUE) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::scale_x_continuous(breaks = 1:12, labels = month_labels) +
      ggplot2::labs(x = "Month", y = units, title = "Seasonal SWOT regime",
                    subtitle = year_subtitle,
                    colour = "Reach", fill = "Reach")
    if (show_n) {
      p <- p + ggplot2::geom_point(aes(size = .data$n_valid), na.rm = TRUE) +
        ggplot2::scale_size_continuous(name = "Valid observations", range = c(1.5, 5))
    } else {
      p <- p + ggplot2::geom_point(na.rm = TRUE)
    }
  } else if (plot == "annual") {
    required <- c("year", "median", "p10", "p90")
    if (!all(required %in% names(tab))) stop("Missing annual columns.", call. = FALSE)
    p <- gg(tab, aes(x = .data$year, y = .data$median,
                     colour = .data$.feature, group = .data$.feature)) +
      ggplot2::geom_linerange(aes(ymin = .data$p10, ymax = .data$p90),
                              linewidth = 0.8, na.rm = TRUE) +
      ggplot2::geom_point(size = 2.5, na.rm = TRUE) +
      ggplot2::scale_x_continuous(breaks = sort(unique(tab$year))) +
      ggplot2::labs(x = "Year", y = units, title = "Annual SWOT regime",
                    subtitle = "Observed annual median and P10–P90 range",
                    colour = "Reach")
  } else if (plot == "relative") {
    required <- c("year", relative_metric)
    if (!all(required %in% names(tab))) stop("Missing relative-condition columns.", call. = FALSE)
    fraction <- relative_metric %in% c("high_fraction", "low_fraction")
    y_label <- switch(relative_metric,
                      median_anomaly = paste0("Median anomaly (", if (variable_type == "wse") "m" else "m³/s", ")"),
                      mean_anomaly = paste0("Mean anomaly (", if (variable_type == "wse") "m" else "m³/s", ")"),
                      standardized_mean_anomaly = "Standardized mean anomaly",
                      high_fraction = "Fraction of high observations",
                      low_fraction = "Fraction of low observations")
    tab$.metric <- tab[[relative_metric]]
    p <- gg(tab, aes(x = .data$year, y = .data$.metric,
                     colour = .data$.feature, group = .data$.feature)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
      ggplot2::geom_point(size = 2.5, na.rm = TRUE) +
      ggplot2::scale_x_continuous(breaks = sort(unique(tab$year))) +
      ggplot2::labs(x = "Year", y = y_label,
                    title = "Relative SWOT hydrological conditions",
                    subtitle = "Relative to the reference distribution supplied to swot_regime()",
                    colour = "Reach")
    if (fraction) p <- p + ggplot2::scale_y_continuous(
      limits = c(0, 1), labels = function(x) paste0(round(x * 100), "%"))
  } else {
    if (!"n_valid" %in% names(tab)) stop("Coverage table lacks n_valid.", call. = FALSE)
    if (coverage_by == "year") {
      if (!"year" %in% names(tab)) stop("Coverage table lacks year.", call. = FALSE)
      p <- gg(tab, aes(x = factor(.data$year), y = .data$n_valid,
                       fill = .data$.feature)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::labs(x = "Year", y = "Valid observations",
                      title = "Annual SWOT observation coverage", fill = "Reach")
    } else {
      if (!"month" %in% names(tab)) stop("Coverage table lacks month.", call. = FALSE)
      tab$.month_label <- factor(month.abb[tab$month], levels = month.abb)
      p <- gg(tab, aes(x = .data$.month_label, y = .data$n_valid,
                       fill = .data$.feature)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::labs(x = "Month (all years combined)", y = "Valid observations",
                      title = "Seasonal SWOT observation coverage", fill = "Reach")
    }
  }
  if (facet && n_features > 1L) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$.feature), scales = "free_y") +
      ggplot2::guides(colour = "none", fill = "none")
  }
  p + ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
