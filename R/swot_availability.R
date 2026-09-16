#' Check SWOT observation availability
#'
#' @description
#' Checks the availability and quality of SWOT observations for each reach
#' or node. The function reports the total number of observations, the number
#' of observations in each quality class, the percentage of good and usable
#' observations, and temporal availability metrics calculated from usable
#' observations only.
#'
#' @param swot_tibble A tibble containing SWOT reach or node observations.
#'   The object must contain either a `reach_id` column or a `node_id` column,
#'   the corresponding quality flag (`reach_q` or `node_q`), and a `time`
#'   column of class POSIXct. If you have been using `swot_download()` to
#'   generate the tibble, the requirements will be met.
#'
#' @author Florian Betz
#'
#' @return
#' A tibble containing observation availability metrics for each reach or node.
#' Temporal interval statistics are calculated using usable observations
#' (quality flags 0, 1, and 2) only.
#'
#' @examples
#' # Download a small sample dataset for a reach of the Drome River
#' # in the Rhone basin, France
#' \dontrun{
#' swot <- swot_download(
#'   id = "21602300301",
#'   type = "Reach",
#'   start_time = "2025-01-01",
#'   end_time = "2025-12-31"
#' )
#'
#' # Run swot_availability
#' swot_availability(swot)
#' }
#'
#' @importFrom rlang .data
#' @export swot_availability
#'

swot_availability <- function(swot_tibble) {

  # --------------------------------------------------------------------------
  # Recognize reach- or node-based tibble
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

  if (!q_col %in% names(swot_tibble)) {
    stop(
      "'swot_tibble' must contain the quality column '",
      q_col,
      "'.",
      call. = FALSE
    )
  }

  if (!"time" %in% names(swot_tibble)) {
    stop(
      "'swot_tibble' must contain a 'time' column.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Summarize availability
  # --------------------------------------------------------------------------

  out <- swot_tibble |>

    dplyr::arrange(
      .data[[id_col]],
      .data$time
    ) |>

    dplyr::group_by(
      .data[[id_col]]
    ) |>

    dplyr::summarise(

      # ----------------------------------------------------------------------
      # Number of observations
      # ----------------------------------------------------------------------

      n_observations = dplyr::n(),

      n_good = sum(
        .data[[q_col]] == 0,
        na.rm = TRUE
      ),

      n_suspect = sum(
        .data[[q_col]] == 1,
        na.rm = TRUE
      ),

      n_degraded = sum(
        .data[[q_col]] == 2,
        na.rm = TRUE
      ),

      n_bad = sum(
        .data[[q_col]] == 3,
        na.rm = TRUE
      ),

      n_usable = sum(
        .data[[q_col]] %in% c(0, 1, 2),
        na.rm = TRUE
      ),


      # ----------------------------------------------------------------------
      # Percentages
      # ----------------------------------------------------------------------

      pct_good = 100 * sum(
        .data[[q_col]] == 0,
        na.rm = TRUE
      ) / dplyr::n(),

      pct_usable = 100 * sum(
        .data[[q_col]] %in% c(0, 1, 2),
        na.rm = TRUE
      ) / dplyr::n(),


      # ----------------------------------------------------------------------
      # Observation period - all observations
      # ----------------------------------------------------------------------

      first_observation = min(
        .data$time,
        na.rm = TRUE
      ),

      last_observation = max(
        .data$time,
        na.rm = TRUE
      ),

      time_span_days = as.numeric(
        difftime(
          max(.data$time, na.rm = TRUE),
          min(.data$time, na.rm = TRUE),
          units = "days"
        )
      ),


      # ----------------------------------------------------------------------
      # Temporal intervals - usable observations only
      # ----------------------------------------------------------------------

      mean_usable_interval_days = {

        usable_time <- .data$time[
          .data[[q_col]] %in% c(0, 1, 2) &
            !is.na(.data$time)
        ]

        if (length(usable_time) > 1) {

          mean(
            as.numeric(
              diff(sort(usable_time)),
              units = "days"
            ),
            na.rm = TRUE
          )

        } else {

          NA_real_
        }
      },

      median_usable_interval_days = {

        usable_time <- .data$time[
          .data[[q_col]] %in% c(0, 1, 2) &
            !is.na(.data$time)
        ]

        if (length(usable_time) > 1) {

          stats::median(
            as.numeric(
              diff(sort(usable_time)),
              units = "days"
            ),
            na.rm = TRUE
          )

        } else {

          NA_real_
        }
      },

      max_usable_gap_days = {

        usable_time <- .data$time[
          .data[[q_col]] %in% c(0, 1, 2) &
            !is.na(.data$time)
        ]

        if (length(usable_time) > 1) {

          max(
            as.numeric(
              diff(sort(usable_time)),
              units = "days"
            ),
            na.rm = TRUE
          )

        } else {

          NA_real_
        }
      },

      .groups = "drop"
    )


  # --------------------------------------------------------------------------
  # Round numeric output to 2 digits
  # --------------------------------------------------------------------------

  out <- out |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~ round(.x, digits = 2)
      )
    )


  # --------------------------------------------------------------------------
  # Return
  # --------------------------------------------------------------------------

  out
}
