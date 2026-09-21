#' Prepare a SWOT time series for further analysis
#'
#' @description
#' Extracts and prepares time series from SWOT RiverSP reach or node
#' observations. The function automatically recognizes reach- or node-based
#' input, optionally selects features and variables, applies quality filtering,
#' and optionally aggregates observations to weekly, monthly, or yearly
#' time steps.
#'
#' @param swot_tibble A tibble containing SWOT RiverSP reach or node
#'   observations. The object must contain either a `reach_id` or `node_id`
#'   column and a `time` column. This is the case if the input was generated
#'   using `swot_download()`.
#'
#' @param id Optional character or numeric vector containing reach or node IDs
#'   to retain. If `NULL`, all features are retained.
#'
#' @param variables Character vector containing variables to retain, for example
#'   `"wse"` or `c("wse", "width")`. If `NULL`, all columns are retained.
#'   Variables must be supplied when temporal aggregation is requested.
#'
#' @param quality Quality classes to retain. One of `"all"`, `"good"`,
#'   `"good_suspect"`, or `"usable"`. `"good"` retains quality flag 0,
#'   `"good_suspect"` retains flags 0 and 1, and `"usable"` retains flags
#'   0, 1, and 2.
#'
#' @param aggregate Temporal aggregation of observations. One of `"none"`,
#'   `"weekly"`, `"monthly"`, or `"yearly"`. The default `"none"` returns
#'   individual SWOT observations.
#'
#' @param agg_fun Aggregation function used when `aggregate` is not `"none"`.
#'   One of `"mean"`, `"median"`, `"min"`, or `"max"`. The default is `"mean"`.
#'   Minimum and maximum represent the minimum or maximum SWOT observation
#'   within the respective period and not necessarily the true hydrological
#'   minimum or maximum.
#'
#' @param format Output format. Either `"wide"` or `"long"`.
#'
#' @author Florian Betz
#'
#' @details
#' Quality filtering is applied before temporal aggregation. Thus, aggregated
#' values are calculated only from observations retained by the selected
#' `quality` criterion.
#'
#' For weekly aggregation, weeks start on Monday. Monthly and yearly periods
#' start on the first day of the respective month or year.
#'
#' When temporal aggregation is requested, uncertainty, quality flags, and
#' observation-specific metadata such as cycle and pass IDs are not returned,
#' because these refer to individual SWOT observations and cannot be directly
#' interpreted for aggregated values.
#'
#' The column `n_obs` gives the number of SWOT observations contributing to
#' each temporal period.
#'
#' @return
#' A tibble containing SWOT observations ordered by feature and time.
#'
#' For non-aggregated output, original observations and associated metadata
#' are retained. In long format, the output contains the columns `variables`,
#' `value`, `uncertainty`, and `quality`.
#'
#' For aggregated output, `time` represents the beginning of the aggregation
#' period and `n_obs` gives the number of SWOT observations in that period.
#' Uncertainty, quality flags, and observation-specific metadata are omitted.
#'
#' @examples
#' swot <- tibble::tibble(
#'   reach_id = rep(c("21602300301", "21602300311"), each = 6),
#'   time = as.POSIXct(
#'     c(
#'       "2025-01-01", "2025-01-12", "2025-01-23",
#'       "2025-02-03", "2025-02-14", "2025-02-25",
#'       "2025-01-03", "2025-01-14", "2025-01-25",
#'       "2025-02-05", "2025-02-16", "2025-02-27"
#'     ),
#'     tz = "UTC"
#'   ),
#'   wse = c(
#'     4.2, 4.4, 4.7, 4.5, 4.8, 5.1,
#'     3.8, 4.0, 4.3, 4.1, 4.5, 4.7
#'   ),
#'   wse_u = c(
#'     0.08, 0.11, 0.07, 0.09, 0.08, 0.10,
#'     0.09, 0.12, 0.08, 0.10, 0.09, 0.11
#'   ),
#'   width = c(
#'     120, 126, 134, 130, 138, 145,
#'     95, 101, 110, 105, 114, 120
#'   ),
#'   width_u = c(
#'     4, 5, 4, 4, 5, 5,
#'     3, 4, 4, 4, 4, 5
#'   ),
#'   reach_q = c(
#'     0, 1, 0, 0, 2, 1,
#'     0, 2, 3, 0, 1, 0
#'   )
#' )
#'
#' # Original observations
#' swot_ts(
#'   swot,
#'   variables = c("wse", "width"),
#'   quality = "usable"
#' )
#'
#' # Long-format observations
#' swot_ts(
#'   swot,
#'   variables = "wse",
#'   quality = "usable",
#'   format = "long"
#' )
#'
#' # Monthly mean water surface elevation
#' swot_ts(
#'   swot,
#'   variables = "wse",
#'   quality = "usable",
#'   aggregate = "monthly",
#'   agg_fun = "mean"
#' )
#'
#' # Monthly maximum observed water surface elevation
#' swot_ts(
#'   swot,
#'   variables = "wse",
#'   quality = "usable",
#'   aggregate = "monthly",
#'   agg_fun = "max"
#' )
#'
#' @importFrom rlang .data :=
#' @export
swot_ts <- function(
    swot_tibble,
    id = NULL,
    variables = NULL,
    quality = c(
      "all",
      "good",
      "good_suspect",
      "usable"
    ),
    aggregate = c(
      "none",
      "weekly",
      "monthly",
      "yearly"
    ),
    agg_fun = c(
      "mean",
      "median",
      "min",
      "max"
    ),
    format = c(
      "wide",
      "long"
    )
) {

  # --------------------------------------------------------------------------
  # Arguments
  # --------------------------------------------------------------------------

  quality <- match.arg(quality)
  aggregate <- match.arg(aggregate)
  agg_fun <- match.arg(agg_fun)
  format <- match.arg(format)


  # --------------------------------------------------------------------------
  # Recognize reach- or node-based input
  # --------------------------------------------------------------------------

  if ("node_id" %in% names(swot_tibble)) {

    id_col <- "node_id"
    q_col <- "node_q"

  } else if ("reach_id" %in% names(swot_tibble)) {

    id_col <- "reach_id"
    q_col <- "reach_q"

  } else {

    stop(
      "'swot_tibble' must contain either a 'node_id' or 'reach_id' column.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Check required columns
  # --------------------------------------------------------------------------

  if (!"time" %in% names(swot_tibble)) {
    stop(
      "'swot_tibble' must contain a 'time' column.",
      call. = FALSE
    )
  }

  if (quality != "all" && !q_col %in% names(swot_tibble)) {
    stop(
      "Quality filtering requires the column '",
      q_col,
      "'.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Select requested IDs
  # --------------------------------------------------------------------------

  out <- swot_tibble

  if (!is.null(id)) {

    id <- as.character(id)

    out <- out |>
      dplyr::filter(
        as.character(.data[[id_col]]) %in% id
      )

    if (nrow(out) == 0) {
      stop(
        "None of the requested IDs were found in 'swot_tibble'.",
        call. = FALSE
      )
    }
  }


  # --------------------------------------------------------------------------
  # Quality filtering
  # --------------------------------------------------------------------------

  if (quality != "all") {

    q_values <- switch(
      quality,
      good = 0,
      good_suspect = c(0, 1),
      usable = c(0, 1, 2)
    )

    out <- out |>
      dplyr::filter(
        .data[[q_col]] %in% q_values
      )
  }


  # --------------------------------------------------------------------------
  # Check requested variables
  # --------------------------------------------------------------------------

  if (!is.null(variables)) {

    variables <- unique(variables)

    missing_variables <- setdiff(
      variables,
      names(out)
    )

    if (length(missing_variables) > 0) {
      stop(
        "Variables not found in 'swot_tibble': ",
        paste(missing_variables, collapse = ", "),
        call. = FALSE
      )
    }
  }


  # --------------------------------------------------------------------------
  # Aggregation requires explicitly selected variables
  # --------------------------------------------------------------------------

  if (aggregate != "none" && is.null(variables)) {
    stop(
      "'variables' must be supplied when temporal aggregation is requested.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Select variables and associated metadata
  # --------------------------------------------------------------------------

  if (!is.null(variables)) {

    # Associated uncertainty columns, e.g. wse -> wse_u
    uncertainty_cols <- paste0(
      variables,
      "_u"
    )

    uncertainty_cols <- intersect(
      uncertainty_cols,
      names(out)
    )

    # Useful SWOT observation metadata
    metadata_cols <- intersect(
      c(
        q_col,
        "cycle_id",
        "pass_id",
        "time_str",
        "sword_version"
      ),
      names(out)
    )

    keep_cols <- unique(
      c(
        id_col,
        "time",
        variables,
        uncertainty_cols,
        metadata_cols
      )
    )

    out <- dplyr::select(
      out,
      dplyr::all_of(keep_cols)
    )
  }


  # --------------------------------------------------------------------------
  # Temporal aggregation
  # --------------------------------------------------------------------------

  if (aggregate != "none") {

    # Determine aggregation period
    period_unit <- switch(
      aggregate,
      weekly = "week",
      monthly = "month",
      yearly = "year"
    )

    # Create start date/time of aggregation period.
    # Weeks start on Monday.
    out <- out |>
      dplyr::mutate(
        .period = lubridate::floor_date(
          .data$time,
          unit = period_unit,
          week_start = 1
        )
      )


    # ------------------------------------------------------------------------
    # Aggregation function
    # ------------------------------------------------------------------------

    agg_function <- switch(
      agg_fun,

      mean = function(x) {
        if (all(is.na(x))) {
          NA_real_
        } else {
          mean(x, na.rm = TRUE)
        }
      },

      median = function(x) {
        if (all(is.na(x))) {
          NA_real_
        } else {
          stats::median(x, na.rm = TRUE)
        }
      },

      min = function(x) {
        if (all(is.na(x))) {
          NA_real_
        } else {
          min(x, na.rm = TRUE)
        }
      },

      max = function(x) {
        if (all(is.na(x))) {
          NA_real_
        } else {
          max(x, na.rm = TRUE)
        }
      }
    )


    # ------------------------------------------------------------------------
    # Aggregate requested variables
    # ------------------------------------------------------------------------

    out <- out |>
      dplyr::group_by(
        .data[[id_col]],
        .data$.period
      ) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(variables),
          agg_function
        ),
        n_obs = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::rename(
        time = .data$.period
      )
  }


  # --------------------------------------------------------------------------
  # Sort observations
  # --------------------------------------------------------------------------

  out <- out |>
    dplyr::arrange(
      .data[[id_col]],
      .data$time
    )


  # --------------------------------------------------------------------------
  # Wide output
  # --------------------------------------------------------------------------

  if (format == "wide") {
    return(
      tibble::as_tibble(out)
    )
  }


  # --------------------------------------------------------------------------
  # Long output
  # --------------------------------------------------------------------------

  if (is.null(variables)) {
    stop(
      "'variables' must be supplied when format = 'long'.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Build long table
  # --------------------------------------------------------------------------

  long_list <- lapply(
    variables,
    function(var) {

      uncertainty_col <- paste0(
        var,
        "_u"
      )

      x <- tibble::tibble(
        !!id_col := out[[id_col]],
        time = out$time,
        variables = var,
        value = out[[var]]
      )


      # ----------------------------------------------------------------------
      # Aggregated output
      # ----------------------------------------------------------------------

      if (aggregate != "none") {

        x$n_obs <- out$n_obs

      } else {

        # --------------------------------------------------------------------
        # Original observation uncertainty
        # --------------------------------------------------------------------

        if (uncertainty_col %in% names(out)) {

          x$uncertainty <- out[[uncertainty_col]]

        } else {

          x$uncertainty <- NA_real_
        }


        # --------------------------------------------------------------------
        # Original observation quality
        # --------------------------------------------------------------------

        if (q_col %in% names(out)) {

          x$quality <- out[[q_col]]

        } else {

          x$quality <- NA_integer_
        }


        # --------------------------------------------------------------------
        # Observation-specific metadata
        # --------------------------------------------------------------------

        for (nm in c(
          "cycle_id",
          "pass_id",
          "time_str",
          "sword_version"
        )) {

          if (nm %in% names(out)) {
            x[[nm]] <- out[[nm]]
          }
        }
      }

      x
    }
  )


  # --------------------------------------------------------------------------
  # Combine long tables
  # --------------------------------------------------------------------------

  out <- dplyr::bind_rows(long_list) |>
    dplyr::arrange(
      .data[[id_col]],
      .data$time,
      .data$variables
    )


  # --------------------------------------------------------------------------
  # Return
  # --------------------------------------------------------------------------

  tibble::as_tibble(out)
}
