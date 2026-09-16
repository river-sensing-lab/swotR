#' Get river names from a SWORD network
#'
#' @description
#' Returns the unique river names contained in a SWORD reach or node dataset.
#' The function automatically detects whether the input contains reaches or
#' nodes and reports the proportion of features for which a river name is
#' available.
#'
#' @param sword A SWORD `sf` object, data frame, or tibble containing either
#'   reaches or nodes.
#'
#' @param name_col Optional name of the river-name column. If `NULL`, the
#'   function attempts to identify a suitable name column automatically.
#'
#' @param sort Logical. If `TRUE`, river names are returned alphabetically.
#'   Default is `TRUE`.
#'
#' @param message Logical. If `TRUE`, report the number of unique river names
#'   and the percentage of the network with an available river name.
#'   Default is `TRUE`.
#'
#' @author Florian Betz
#'
#' @return
#' A character vector containing unique river names.
#'
#' @examples
#' \dontrun{
#' sword <- sword_download(
#'   ...,
#'   feature = "Reach"
#' )
#'
#' sword_names(sword)
#'
#' names <- sword_names(
#'   sword,
#'   message = FALSE
#' )
#' }
#'
#' @export
sword_names <- function(
    sword,
    name_col = NULL,
    sort = TRUE,
    message = TRUE
) {

  # --------------------------------------------------------------------------
  # Recognize reach- or node-based input
  # --------------------------------------------------------------------------

  if ("node_id" %in% names(sword)) {

    feature_type <- "nodes"
    id_col <- "node_id"

  } else if ("reach_id" %in% names(sword)) {

    feature_type <- "reaches"
    id_col <- "reach_id"

  } else {

    stop(
      "'sword' must contain either a 'node_id' or 'reach_id' column.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Identify river-name column
  # --------------------------------------------------------------------------

  if (is.null(name_col)) {

    name_candidates <- c(
      "river_name",
      "river_names",
      "name"
    )

    name_matches <- name_candidates[
      name_candidates %in% names(sword)
    ]

    if (length(name_matches) == 0) {
      stop(
        "No river-name column could be identified in 'sword'. ",
        "Supply its name using 'name_col'.",
        call. = FALSE
      )
    }

    name_col <- name_matches[1]

  } else {

    if (!name_col %in% names(sword)) {
      stop(
        "Name column '",
        name_col,
        "' was not found in 'sword'.",
        call. = FALSE
      )
    }
  }


  # --------------------------------------------------------------------------
  # Prepare names
  # --------------------------------------------------------------------------

  river_name <- trimws(
    as.character(
      sword[[name_col]]
    )
  )

  named <- !is.na(river_name) &
    river_name != "" &
    !river_name %in% c(
      "nan",
      "none",
      "na",
      "NODATA",
      NA
    )


  # --------------------------------------------------------------------------
  # Unique river names
  # --------------------------------------------------------------------------

  river_names <- unique(
    river_name[named]
  )

  if (sort) {
    river_names <- base::sort(
      river_names
    )
  }


  # --------------------------------------------------------------------------
  # Calculate naming coverage
  # --------------------------------------------------------------------------

  n_features <- dplyr::n_distinct(
    sword[[id_col]],
    na.rm = TRUE
  )

  n_named <- dplyr::n_distinct(
    sword[[id_col]][named],
    na.rm = TRUE
  )

  pct_named <- if (n_features > 0) {
    100 * n_named / n_features
  } else {
    NA_real_
  }


  # --------------------------------------------------------------------------
  # Message
  # --------------------------------------------------------------------------

  if (message) {

    message(
      "Found ",
      length(river_names),
      " unique river name",
      if (length(river_names) == 1) "" else "s",
      ". ",
      round(pct_named, 1),
      "% of ",
      feature_type,
      " have a river name."
    )
  }


  # --------------------------------------------------------------------------
  # Return
  # --------------------------------------------------------------------------

  river_names
}
