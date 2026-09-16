#' Prepare a SWOT time series for further analysis
#'
#' @description
#' Extracts and prepares time series from SWOT RiverSP reach or node
#' observations. The function automatically recognizes reach- or node-based
#' input, optionally selects features and variabless, applies quality filtering,
#' and returns observations ordered by feature and time.
#'
#' @param swot_tibble A tibble containing SWOT RiverSP reach or node
#'   observations. The object must contain either a `reach_id` or `node_id`
#'   column and a `time` column. This is the case, if you generated the input th
#'   through the swot_download() function.
#'
#' @param id Optional character or numeric vector containing reach or node IDs
#'   to retain. If `NULL`, all features are retained.
#'
#' @param variables Character vector containing variabless to retain, for example
#'   `"wse"` or `c("wse", "width")`. If `NULL`, all columns are retained.
#'
#' @param quality Quality classes to retain. One of `"all"`, `"good"`,
#'   `"good_suspect"`, or `"usable"`. `"good"` retains quality flag 0,
#'   `"good_suspect"` retains flags 0 and 1, and `"usable"` retains flags
#'   0, 1, and 2.
#'
#' @param format Output format. Either `"wide"` or `"long"`.
#'
#'@author Florian Betz
#'
#' @return
#' A tibble containing SWOT observations ordered by feature and time.
#' In long format, the output contains the columns `variables`, `value`,
#' `uncertainty`, and `quality`.
#'
#' @examples
#' swot <- tibble::tibble(
#'   reach_id = rep(c("21602300301", "21602300311"), each = 3),
#'   time = as.POSIXct(
#'     c(
#'       "2025-01-01", "2025-01-12", "2025-01-23",
#'       "2025-01-03", "2025-01-14", "2025-01-25"
#'     ),
#'     tz = "UTC"
#'   ),
#'   wse = c(4.2, 4.4, 4.7, 3.8, 4.0, 4.3),
#'   wse_u = c(0.08, 0.11, 0.07, 0.09, 0.12, 0.08),
#'   width = c(120, 126, 134, 95, 101, 110),
#'   width_u = c(4, 5, 4, 3, 4, 4),
#'   reach_q = c(0, 1, 0, 0, 2, 3)
#' )
#'
#' swot_ts(
#'   swot,
#'   variables = c("wse", "width"),
#'   quality = "usable"
#' )
#'
#' swot_ts(
#'   swot,
#'   variables = "wse",
#'   quality = "usable",
#'   format = "long"
#' )
#'
#' @importFrom rlang .data :=
#' @export swot_ts
#'

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
    format = c(
      "wide",
      "long"
    )
) {

  # --------------------------------------------------------------------------
  # Arguments
  # --------------------------------------------------------------------------

  quality <- match.arg(quality)
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
  # Check requested variabless
  # --------------------------------------------------------------------------

  if (!is.null(variables)) {

    variables <- unique(variables)

    missing_variabless <- setdiff(
      variables,
      names(out)
    )

    if (length(missing_variabless) > 0) {
      stop(
        "variabless not found in 'swot_tibble': ",
        paste(missing_variabless, collapse = ", "),
        call. = FALSE
      )
    }
  }


  # --------------------------------------------------------------------------
  # Select variabless and associated metadata
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

  # Build the long table explicitly so uncertainty values can be matched
  # to their corresponding variabless.

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

      if (uncertainty_col %in% names(out)) {

        x$uncertainty <- out[[uncertainty_col]]

      } else {

        x$uncertainty <- NA_real_
      }

      if (q_col %in% names(out)) {

        x$quality <- out[[q_col]]

      } else {

        x$quality <- NA_integer_
      }

      # Retain common observation metadata
      for (nm in c("cycle_id", "pass_id", "time_str", "sword_version")) {

        if (nm %in% names(out)) {
          x[[nm]] <- out[[nm]]
        }
      }

      x
    }
  )

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
