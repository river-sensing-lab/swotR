#' Create a SWOT river space-time representation
#'
#' @description
#' Creates a longitudinal space-time representation of SWOT RiverSP
#' observations. SWOT observations are joined with the corresponding SWORD
#' network to obtain hydrological distance from the outlet. Observations are
#' aggregated into temporal bins and can be returned as a tibble or displayed
#' as a heatmap.
#'
#' @param swot_tibble A SWOT RiverSP tibble containing node or reach
#'   observations.
#'
#' @param sword Corresponding SWORD nodes or reaches. May be an `sf` object,
#'   data frame, or tibble.
#'
#' @param variable Variable to display. One of `"wse"`, `"width"`, or
#'   `"area_total"`.
#'
#' @param bin_days Width of the temporal bins in days. Default is `1`.
#'
#' @param quality Quality levels to retain. One of `"all"`, `"good"`,
#'   `"good_suspect"`, or `"usable"`. Default is `"usable"`.
#'
#' @param distance_col Optional name of the SWORD hydrological-distance
#'   column. If `NULL`, the function attempts to identify it automatically.
#'
#' @param distance_unit Unit used for outlet distance. Either `"km"` or `"m"`.
#'   Default is `"km"`.
#'
#' @param aggregation Function used when multiple observations occur in the
#'   same temporal bin at the same river location. Either `"mean"` or
#'   `"median"`. Default is `"mean"`.
#'
#' @param plot Logical. If `TRUE`, return a `ggplot` heatmap. If `FALSE`,
#'   return the aggregated space-time tibble.
#'
#' @param tile_width plotting with of each data entry, default is 1.5
#'
#' @author Florian Betz
#'
#' @return
#' A `ggplot` object if `plot = TRUE`, otherwise a tibble containing the
#' aggregated space-time observations.
#'
#' @examples
#' \dontrun{
#' swot_spacetime(
#'   rhone_swot,
#'   rhone_sword,
#'   variable = "wse"
#' )
#'
#' swot_spacetime(
#'   rhone_swot,
#'   rhone_sword,
#'   variable = "width",
#'   bin_days = 5
#' )
#' }
#'
#' @importFrom rlang .data
#' @export swot_spacetime
#'

swot_spacetime <- function(
    swot_tibble,
    sword,
    variable = c("wse", "width", "area_total"),
    bin_days = 1,
    quality = c(
      "usable",
      "good",
      "good_suspect",
      "all"
    ),
    distance_col = NULL,
    distance_unit = c("km", "m"),
    aggregation = c("mean", "median"),
    plot = TRUE,
    tile_width=1.5
) {

  # --------------------------------------------------------------------------
  # Arguments
  # --------------------------------------------------------------------------

  variable <- match.arg(variable)
  quality <- match.arg(quality)
  distance_unit <- match.arg(distance_unit)
  aggregation <- match.arg(aggregation)

  if (!is.numeric(bin_days) ||
      length(bin_days) != 1 ||
      is.na(bin_days) ||
      bin_days < 1) {
    stop(
      "'bin_days' must be a positive number.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Recognize node- or reach-based SWOT input
  # --------------------------------------------------------------------------

  if ("node_id" %in% names(swot_tibble)) {

    feature_type <- "Node"
    id_col <- "node_id"
    q_col <- "node_q"

  } else if ("reach_id" %in% names(swot_tibble)) {

    feature_type <- "Reach"
    id_col <- "reach_id"
    q_col <- "reach_q"

  } else {

    stop(
      "'swot_tibble' must contain either 'node_id' or 'reach_id'.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Check required SWOT variables
  # --------------------------------------------------------------------------

  if (!"time" %in% names(swot_tibble)) {
    stop(
      "'swot_tibble' must contain a 'time' column.",
      call. = FALSE
    )
  }

  if (!variable %in% names(swot_tibble)) {
    stop(
      "Variable '",
      variable,
      "' was not found in 'swot_tibble'.",
      call. = FALSE
    )
  }

  if (!q_col %in% names(swot_tibble) &&
      quality != "all") {
    stop(
      "Quality column '",
      q_col,
      "' was not found in 'swot_tibble'.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Prepare SWORD
  # --------------------------------------------------------------------------

  if (inherits(sword, "sf")) {
    sword <- sf::st_drop_geometry(sword)
  }

  if (!id_col %in% names(sword)) {
    stop(
      "'sword' does not contain the required '",
      id_col,
      "' column.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Identify outlet-distance column
  # --------------------------------------------------------------------------

  if (is.null(distance_col)) {

    distance_candidates <- c(
      "hydro_dist_out",
      "dist_out",
      "dist_out_dijkstra",
      "longitudinal_distance"
    )

    distance_matches <- distance_candidates[
      distance_candidates %in% names(sword)
    ]

    if (length(distance_matches) == 0) {
      stop(
        "No outlet-distance column could be identified in 'sword'. ",
        "Supply its name using 'distance_col'.",
        call. = FALSE
      )
    }

    distance_col <- distance_matches[1]

  } else {

    if (!distance_col %in% names(sword)) {
      stop(
        "Distance column '",
        distance_col,
        "' was not found in 'sword'.",
        call. = FALSE
      )
    }
  }


  # --------------------------------------------------------------------------
  # Prepare distance lookup
  # --------------------------------------------------------------------------

  distance_lookup <- sword |>
    dplyr::select(
      dplyr::all_of(
        c(
          id_col,
          distance_col
        )
      )
    ) |>
    dplyr::distinct(
      .data[[id_col]],
      .keep_all = TRUE
    )

  distance_lookup[[id_col]] <-
    as.character(
      distance_lookup[[id_col]]
    )

  names(distance_lookup)[
    names(distance_lookup) == distance_col
  ] <- ".distance"


  # --------------------------------------------------------------------------
  # Prepare SWOT IDs
  # --------------------------------------------------------------------------

  dat <- swot_tibble

  dat[[id_col]] <-
    as.character(
      dat[[id_col]]
    )


  # --------------------------------------------------------------------------
  # Quality filtering
  # --------------------------------------------------------------------------

  if (quality != "all") {

    quality_values <- switch(
      quality,
      good = 0,
      good_suspect = c(0, 1),
      usable = c(0, 1, 2)
    )

    dat <- dat |>
      dplyr::filter(
        .data[[q_col]] %in%
          quality_values
      )
  }


  # --------------------------------------------------------------------------
  # Join outlet distance
  # --------------------------------------------------------------------------

  dat <- dat |>
    dplyr::left_join(
      distance_lookup,
      by = id_col
    )

  if (all(is.na(dat$.distance))) {
    stop(
      "No SWOT features could be matched to SWORD.",
      call. = FALSE
    )
  }

  n_missing_distance <- sum(
    is.na(dat$.distance)
  )

  if (n_missing_distance > 0) {
    warning(
      n_missing_distance,
      " SWOT observations could not be matched to a SWORD distance.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Convert distance
  # --------------------------------------------------------------------------

  if (distance_unit == "km") {
    dat$.distance <-
      dat$.distance / 1000
  }


  # --------------------------------------------------------------------------
  # Create temporal bins
  # --------------------------------------------------------------------------

  dat$.date <- as.Date(
    dat$time,
    tz = "UTC"
  )

  origin <- min(
    dat$.date,
    na.rm = TRUE
  )

  dat$.time_bin <- origin +
    floor(
      as.numeric(
        dat$.date - origin
      ) / bin_days
    ) * bin_days


  # --------------------------------------------------------------------------
  # Aggregate observations
  # --------------------------------------------------------------------------

  if (aggregation == "mean") {

    out <- dat |>
      dplyr::filter(
        !is.na(.data$.distance),
        !is.na(.data[[variable]]),
        !is.na(.data$.time_bin)
      ) |>
      dplyr::group_by(
        .data$.time_bin,
        .data$.distance
      ) |>
      dplyr::summarise(
        value = mean(
          .data[[variable]],
          na.rm = TRUE
        ),
        n_observations = dplyr::n(),
        .groups = "drop"
      )

  } else {

    out <- dat |>
      dplyr::filter(
        !is.na(.data$.distance),
        !is.na(.data[[variable]]),
        !is.na(.data$.time_bin)
      ) |>
      dplyr::group_by(
        .data$.time_bin,
        .data$.distance
      ) |>
      dplyr::summarise(
        value = stats::median(
          .data[[variable]],
          na.rm = TRUE
        ),
        n_observations = dplyr::n(),
        .groups = "drop"
      )
  }


  # --------------------------------------------------------------------------
  # Standardize output names
  # --------------------------------------------------------------------------

  out <- out |>
    dplyr::rename(
      time_bin = .data$.time_bin,
      distance = .data$.distance
    ) |>
    dplyr::arrange(
      .data$time_bin,
      dplyr::desc(.data$distance)
    )


  # --------------------------------------------------------------------------
  # Return data
  # --------------------------------------------------------------------------

  if (!plot) {

    attr(out, "feature_type") <- feature_type
    attr(out, "variable") <- variable
    attr(out, "distance_unit") <- distance_unit
    attr(out, "distance_source") <- distance_col
    attr(out, "bin_days") <- bin_days

    return(
      tibble::as_tibble(out)
    )
  }


  # --------------------------------------------------------------------------
  # Plot labels
  # --------------------------------------------------------------------------

  x_label <- if (distance_unit == "km") {
    "Distance to outlet [km]"
  } else {
    "Distance to outlet [m]"
  }

  fill_label <- switch(
    variable,
    wse = "Water surface elevation [m]",
    width = "River width [m]",
    area_total = "Water surface area [m2]"
  )


  # --------------------------------------------------------------------------
  # Heatmap
  # --------------------------------------------------------------------------

  p <- ggplot2::ggplot(
    out,
    ggplot2::aes(
      x = .data$distance,
      y = .data$time_bin,
      fill = .data$value
    )
  ) +
    ggplot2::geom_tile(width=tile_width) +
    ggplot2::scale_fill_viridis_c(trans="log10")+
    ggplot2::scale_x_reverse() +
    ggplot2::labs(
      x = x_label,
      y = "Date",
      fill = fill_label
    ) +
    ggplot2::theme_minimal()

  p
}
