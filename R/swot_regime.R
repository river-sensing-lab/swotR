#' Summarise SWOT water-level or discharge regimes
#'
#' @description Computes SWOT-adapted regime, annual and relative-condition
#'   indicators from irregular RiverSP observations. The outputs are descriptive
#'   of the sampled observations, not continuous daily hydrological indices.
#' @param data A data.frame/tibble or sf object returned by swot_download().
#' @param input Either "wse" or "discharge".
#' @param variable Numeric column to analyse. Defaults to "wse" or
#'   "sos_consensus_q", respectively.
#' @param quality One of "all", "good", "good_suspect", "usable". Filters
#'   reach_q (or node_q) using 0, 0:1, or 0:2, respectively, when present.
#' @param reference "full" for all valid observations, or a numeric vector
#'   of years (e.g., 2024:2025) defining the reference distribution.
#' @param min_obs Minimum valid observations needed to report statistics
#'   for each group/year; groups below the threshold retain diagnostics.
#' @param min_months Minimum distinct calendar months with observations
#'   needed to report annual or overall statistics.
#' @param high_prob,low_prob Reference exceedance quantile probabilities.
#' @param max_gap_days Maximum interval between consecutive observations
#'   used for optional rise/fall-rate diagnostics; NULL disables rates.
#' @param id_col Optional grouping ID column, automatically inferred from
#'   reach_id or node_id; NULL if neither is present.
#' @param quiet Suppress informative warnings about missing quality columns.
#' @return A list of tibbles: `regime` (whole-record summary), `annual`
#'   (yearly summaries), `relative` (yearly reference-relative indicators), and `seasonal`
#'   (calendar-month summaries across the observation period).
#'   Quantiles, extrema and exceedance frequencies describe sampled observations.
#' @export
swot_regime <- function(data,
                        input = c("wse", "discharge"),
                        variable = NULL,
                        quality = c("usable", "all", "good", "good_suspect"),
                        reference = "full",
                        min_obs = 10L,
                        min_months = 6L,
                        high_prob = 0.9,
                        low_prob = 0.1,
                        max_gap_days = NULL,
                        id_col = NULL,
                        quiet = FALSE) {
  input <- match.arg(input)
  quality <- match.arg(quality)
  if (!is.data.frame(data)) stop("'data' must be a data.frame or sf object.", call. = FALSE)
  if (!is.numeric(min_obs) || length(min_obs) != 1L || is.na(min_obs) || min_obs < 1 ||
      !is.numeric(min_months) || length(min_months) != 1L || is.na(min_months) ||
      min_months < 1 || min_months > 12) {
    stop("'min_obs' must be >= 1 and 'min_months' must be between 1 and 12.", call. = FALSE)
  }
  if (!is.numeric(high_prob) || !is.numeric(low_prob) || length(high_prob) != 1L ||
      length(low_prob) != 1L || is.na(high_prob) || is.na(low_prob) ||
      low_prob <= 0 || high_prob >= 1 || low_prob >= high_prob) {
    stop("Require 0 < low_prob < high_prob < 1.", call. = FALSE)
  }
  if (!is.null(max_gap_days) && (!is.numeric(max_gap_days) ||
                                 length(max_gap_days) != 1L || !is.finite(max_gap_days) || max_gap_days <= 0)) {
    stop("'max_gap_days' must be NULL or a positive number.", call. = FALSE)
  }
  if (!(identical(reference, "full") ||
        (is.numeric(reference) && length(reference) > 0L && all(is.finite(reference))))) {
    stop("'reference' must be 'full' or a numeric vector of years.", call. = FALSE)
  }
  if (is.null(variable)) variable <- if (input == "wse") "wse" else "sos_consensus_q"
  if (!is.character(variable) || length(variable) != 1L || !variable %in% names(data)) {
    stop("Selected 'variable' is not a column in 'data'.", call. = FALSE)
  }
  if (is.null(id_col)) {
    id_col <- if ("reach_id" %in% names(data)) "reach_id" else
      if ("node_id" %in% names(data)) "node_id" else NULL
  }
  if (!is.null(id_col) && (!is.character(id_col) || length(id_col) != 1L ||
                           !id_col %in% names(data))) {
    stop("'id_col' must name a column in 'data'.", call. = FALSE)
  }
  if (!"time" %in% names(data) && !"time_str" %in% names(data)) {
    stop("'data' needs 'time' or 'time_str'.", call. = FALSE)
  }
  # Avoid geometry columns interfering with grouping and bind_rows.
  if (inherits(data, "sf")) data <- sf::st_drop_geometry(data)
  dat <- tibble::as_tibble(data)
  if (!"time" %in% names(dat)) dat$time <- dat$time_str
  if (!inherits(dat$time, "POSIXt")) {
    dat$time <- as.POSIXct(dat$time, tz = "UTC", tryFormats = c(
      "%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%OS%z",
      "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d"))
  }
  if (all(is.na(dat$time)) && nrow(dat) > 0L) {
    stop("No valid observation times could be parsed.", call. = FALSE)
  }
  if (quality != "all") {
    qcol <- if ("reach_q" %in% names(dat)) "reach_q" else
      if ("node_q" %in% names(dat)) "node_q" else NULL
    if (is.null(qcol)) {
      if (!quiet) warning("No reach_q/node_q column: quality filtering skipped.", call. = FALSE)
    } else {
      allowed <- switch(quality, good = 0, good_suspect = 0:1, usable = 0:2)
      dat <- dat[!is.na(dat[[qcol]]) & dat[[qcol]] %in% allowed, , drop = FALSE]
    }
  }
  dat$.value <- suppressWarnings(as.numeric(dat[[variable]]))
  dat$.value[!is.finite(dat$.value) | dat$.value == -999999999999] <- NA_real_
  dat <- dat[!is.na(dat$time), , drop = FALSE]
  dat$.year <- as.integer(format(dat$time, "%Y", tz = "UTC"))
  dat$.month <- as.integer(format(dat$time, "%m", tz = "UTC"))
  dat$.group <- if (is.null(id_col)) rep("all", nrow(dat)) else as.character(dat[[id_col]])
  dat$.group[is.na(dat$.group)] <- "NA"
  groups <- split(dat, dat$.group, drop = TRUE)

  empty <- function() tibble::tibble()
  if (!length(groups)) return(list(regime = empty(), annual = empty(), relative = empty(), seasonal = empty()))

  # Summaries are observation-based; a minimum number of distinct months is
  # used to avoid presenting a seasonal fragment as an annual regime.
  summarise_one <- function(d, id, year = NA_integer_) {
    d <- d[order(d$time), , drop = FALSE]
    valid <- d[!is.na(d$.value), , drop = FALSE]
    n <- nrow(valid)
    months <- length(unique(valid$.month))
    enough <- n >= min_obs && months >= min_months
    v <- valid$.value
    q <- function(p) as.numeric(stats::quantile(v, p, names = FALSE, na.rm = TRUE))
    med <- if (enough) stats::median(v) else NA_real_
    sdv <- if (enough && n > 1L) stats::sd(v) else NA_real_
    # WSE CV is datum-dependent and is deliberately omitted.
    cv <- if (input == "discharge" && enough && is.finite(mean(v)) && mean(v) != 0 &&
              n > 1L) sdv / mean(v) else NA_real_
    gaps <- if (n > 1L) as.numeric(diff(sort(valid$time)), units = "days") else numeric()
    gaps <- gaps[is.finite(gaps)]
    rates <- numeric()
    if (!is.null(max_gap_days) && n > 1L) {
      dv <- diff(v)
      dt <- as.numeric(diff(valid$time), units = "days")
      ok <- is.finite(dt) & dt > 0 & dt <= max_gap_days
      rates <- dv[ok] / dt[ok]
    }
    tibble::tibble(
      group_id = id, year = year, input = input, variable = variable,
      n_obs = nrow(d), n_valid = n, n_months = months, sufficient = enough,
      median_gap_days = if (length(gaps)) stats::median(gaps) else NA_real_,
      max_gap_days = if (length(gaps)) max(gaps) else NA_real_,
      mean = if (enough) mean(v) else NA_real_,
      median = med, sd = sdv, cv = cv,
      p10 = if (enough) q(0.1) else NA_real_,
      p90 = if (enough) q(0.9) else NA_real_,
      iqr = if (enough) stats::IQR(v) else NA_real_,
      min = if (enough) min(v) else NA_real_,
      max = if (enough) max(v) else NA_real_,
      amplitude = if (enough) diff(range(v)) else NA_real_,
      month_min = if (enough) valid$.month[which.min(v)] else NA_integer_,
      month_max = if (enough) valid$.month[which.max(v)] else NA_integer_,
      rise_rate_mean = if (enough && any(rates > 0)) mean(rates[rates > 0]) else NA_real_,
      fall_rate_mean = if (enough && any(rates < 0)) mean(rates[rates < 0]) else NA_real_,
      zero_fraction = if (input == "discharge" && enough) mean(v == 0) else NA_real_
    )
  }

  regime <- dplyr::bind_rows(lapply(names(groups), function(id) {
    summarise_one(groups[[id]], id)
  }))
  annual <- dplyr::bind_rows(lapply(names(groups), function(id) {
    ys <- split(groups[[id]], groups[[id]]$.year)
    dplyr::bind_rows(lapply(names(ys), function(y) {
      summarise_one(ys[[y]], id, as.integer(y))
    }))
  }))
  # Reference quantiles use valid observations only; reference sufficiency is
  # checked separately, and missing reference data never produce a threshold.
  relative <- dplyr::bind_rows(lapply(names(groups), function(id) {
    d <- groups[[id]]
    ref <- if (identical(reference, "full")) d else d[d$.year %in% reference, , drop = FALSE]
    ref <- ref[!is.na(ref$.value), , drop = FALSE]
    ref_ok <- nrow(ref) >= min_obs && length(unique(ref$.month)) >= min_months
    rmedian <- if (ref_ok) stats::median(ref$.value) else NA_real_
    rmean <- if (ref_ok) mean(ref$.value) else NA_real_
    rsd <- if (ref_ok && nrow(ref) > 1L) stats::sd(ref$.value) else NA_real_
    lo <- if (ref_ok) as.numeric(stats::quantile(ref$.value, low_prob)) else NA_real_
    hi <- if (ref_ok) as.numeric(stats::quantile(ref$.value, high_prob)) else NA_real_
    ys <- split(d, d$.year)
    dplyr::bind_rows(lapply(names(ys), function(y) {
      a <- ys[[y]]
      v <- a$.value[!is.na(a$.value)]
      n <- length(v)
      nmonths <- length(unique(a$.month[!is.na(a$.value)]))
      ok <- ref_ok && n >= min_obs && nmonths >= min_months
      tibble::tibble(
        group_id = id, year = as.integer(y), input = input, variable = variable,
        n_valid = n, n_months = nmonths, reference_n = nrow(ref),
        reference_median = rmedian, reference_low = lo, reference_high = hi,
        sufficient = ok,
        median_anomaly = if (ok) stats::median(v) - rmedian else NA_real_,
        mean_anomaly = if (ok) mean(v) - rmean else NA_real_,
        standardized_mean_anomaly = if (ok && is.finite(rsd) && rsd > 0)
          (mean(v) - rmean) / rsd else NA_real_,
        high_fraction = if (ok) mean(v > hi) else NA_real_,
        low_fraction = if (ok) mean(v < lo) else NA_real_
      )
    }))
  }))
  seasonal <- dplyr::bind_rows(lapply(names(groups), function(id) {
    ms <- split(groups[[id]], groups[[id]]$.month)
    dplyr::bind_rows(lapply(names(ms), function(m) {
      d <- ms[[m]]
      v <- d$.value[!is.na(d$.value)]
      tibble::tibble(group_id = id, month = as.integer(m), input = input,
                     variable = variable, n_obs = nrow(d), n_valid = length(v),
                     n_years = length(unique(d$.year[!is.na(d$.value)])),
                     median = if (length(v) >= min_obs) stats::median(v) else NA_real_,
                     p10 = if (length(v) >= min_obs)
                       as.numeric(stats::quantile(v, 0.1)) else NA_real_,
                     p90 = if (length(v) >= min_obs)
                       as.numeric(stats::quantile(v, 0.9)) else NA_real_)
    }))
  }))
  if (!is.null(id_col)) {
    names(seasonal)[names(seasonal) == "group_id"] <- id_col
    names(regime)[names(regime) == "group_id"] <- id_col
    names(annual)[names(annual) == "group_id"] <- id_col
    names(relative)[names(relative) == "group_id"] <- id_col
  }
  list(regime = regime, annual = annual, relative = relative, seasonal = seasonal)
}
