#' Prepare a longitudinal SWOT river profile
#'
#' @description
#' Extracts a longitudinal profile from SWOT RiverSP reach or node
#' observations. The function joins SWOT observations with the corresponding
#' SWORD nodes or reaches to obtain distance to the outlet, selects observations
#' from a SWOT cycle/pass or around a specified time, optionally applies
#' quality filtering, and orders observations longitudinally from upstream
#' to downstream.
#'
#' Optionally, the function can return a longitudinal `ggplot` for a selected
#' variable.
#'
#' @param swot_tibble A tibble containing SWOT RiverSP reach or node
#'   observations. The object must contain either a `reach_id` or `node_id`
#'   column and a `time` column.
#'
#' @param sword A data frame, tibble, or `sf` object containing the
#'   corresponding SWORD reaches or nodes. It must contain the same identifier
#'   (`reach_id` or `node_id`) as `swot_tibble` and a downstream-distance
#'   variable.
#'
#' @param cycle_id Optional SWOT cycle identifier.
#'
#' @param pass_id Optional SWOT pass identifier. Usually supplied together
#'   with `cycle_id`.
#'
#' @param time Optional target time. If supplied, observations within
#'   `time_tolerance` of this time are retained.
#'
#' @param time_tolerance Maximum temporal difference from `time`, in minutes.
#'   Default is 30 minutes.
#'
#' @param variable Character vector containing variables to retain, for example
#'   `"wse"` or `c("wse", "width")`. If `NULL`, all variables are retained.
#'
#' @param quality Quality classes to retain. One of `"all"`, `"good"`,
#'   `"good_suspect"`, or `"usable"`. `"good"` retains quality flag 0,
#'   `"good_suspect"` retains flags 0 and 1, and `"usable"` retains flags
#'   0, 1, and 2.
#'
#' @param distance_col Name of the SWORD column containing distance to the
#'   outlet. If `NULL`, the function attempts to identify a suitable distance
#'   column automatically.
#'
#' @param distance_unit Unit of the returned longitudinal distance.
#'   Either `"km"` or `"m"`.
#'
#' @param plot Logical. If `TRUE`, return a longitudinal profile plot instead
#'   of the profile tibble. Default is `FALSE`.
#'
#' @param plot_variable Character string specifying the variable to plot when
#'   `plot = TRUE`. If `NULL` and exactly one `variable` is selected, that
#'   variable is used automatically.
#'
#' @author Florian Betz
#'
#' @return
#' If `plot = FALSE`, a tibble containing SWOT observations ordered
#' longitudinally from upstream to downstream. The column `distance` contains
#' distance to the outlet in the requested unit.
#'
#' If `plot = TRUE`, a `ggplot` object showing the selected variable along
#' the longitudinal river profile.
#'
#' @examples
#' swot <- tibble::tibble(
#'   node_id = c("1", "2", "3", "4"),
#'   reach_id = rep("21602300301", 4),
#'   time = as.POSIXct(
#'     c(
#'       "2025-01-12 10:00:01",
#'       "2025-01-12 10:00:03",
#'       "2025-01-12 10:00:06",
#'       "2025-01-12 10:00:08"
#'     ),
#'     tz = "UTC"
#'   ),
#'   cycle_id = rep(40, 4),
#'   pass_id = rep(15, 4),
#'   wse = c(105.2, 104.7, 104.1, 103.5),
#'   wse_u = c(0.08, 0.07, 0.10, 0.09),
#'   width = c(180, 195, 210, 225),
#'   width_u = c(4, 5, 4, 3),
#'   node_q = c(0, 0, 1, 2)
#' )
#'
#' sword <- tibble::tibble(
#'   node_id = c("1", "2", "3", "4"),
#'   hydro_dist_out = c(40000, 30000, 20000, 10000)
#' )
#'
#' swot_profile(
#'   swot,
#'   sword,
#'   cycle_id = 40,
#'   pass_id = 15,
#'   variable = c("wse", "width"),
#'   quality = "usable"
#' )
#'
#' swot_profile(
#'   swot,
#'   sword,
#'   cycle_id = 40,
#'   pass_id = 15,
#'   variable = c("wse", "width"),
#'   quality = "usable",
#'   plot = TRUE,
#'   plot_variable = "wse"
#' )
#'
#' @importFrom rlang .data
#' @export swot_profile
#'

swot_profile <- function(
    swot_tibble,
    sword,
    cycle_id = NULL,
    pass_id = NULL,
    time = NULL,
    time_tolerance = 1440,
    variable = NULL,
    quality = c(
      "all",
      "good",
      "good_suspect",
      "usable"
    ),
    distance_col = NULL,
    distance_unit = c(
      "km",
      "m"
    ),
    plot = FALSE,
    plot_variable = NULL
) {

  # --------------------------------------------------------------------------
  # Arguments
  # --------------------------------------------------------------------------

  quality <- match.arg(quality)
  distance_unit <- match.arg(distance_unit)


  # --------------------------------------------------------------------------
  # Recognize reach- or node-based SWOT input
  # --------------------------------------------------------------------------

  if ("node_id" %in% names(swot_tibble)) {

    id_col <- "node_id"
    q_col <- "node_q"
    feature_type <- "Node"

  } else if ("reach_id" %in% names(swot_tibble)) {

    id_col <- "reach_id"
    q_col <- "reach_q"
    feature_type <- "Reach"

  } else {

    stop(
      "'swot_tibble' must contain either a 'node_id' or 'reach_id' column.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Check required SWOT columns
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
  # Prepare SWORD data
  # --------------------------------------------------------------------------

  if (!id_col %in% names(sword)) {
    stop(
      "'sword' must contain the column '",
      id_col,
      "' because the SWOT input contains ",
      tolower(feature_type),
      " observations.",
      call. = FALSE
    )
  }

  # Remove geometry before joining
  if (inherits(sword, "sf")) {
    sword <- sf::st_drop_geometry(sword)
  }


  # --------------------------------------------------------------------------
  # Identify downstream-distance column
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
        "No downstream-distance column could be identified in 'sword'. ",
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
  # Prepare SWORD join table
  # --------------------------------------------------------------------------

  sword_join <- sword |>
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


  # --------------------------------------------------------------------------
  # Ensure matching ID types
  # --------------------------------------------------------------------------

  swot_tibble[[id_col]] <- as.character(
    swot_tibble[[id_col]]
  )

  sword_join[[id_col]] <- as.character(
    sword_join[[id_col]]
  )


  # --------------------------------------------------------------------------
  # Join SWOT observations with SWORD
  # --------------------------------------------------------------------------

  out <- swot_tibble |>
    dplyr::left_join(
      sword_join,
      by = id_col
    )


  # --------------------------------------------------------------------------
  # Check joined distances
  # --------------------------------------------------------------------------

  n_missing_distance <- sum(
    is.na(out[[distance_col]])
  )

  if (n_missing_distance == nrow(out)) {
    stop(
      "No SWOT feature IDs could be matched to the supplied SWORD data.",
      call. = FALSE
    )
  }

  if (n_missing_distance > 0) {
    warning(
      n_missing_distance,
      " SWOT observation(s) could not be matched to a SWORD distance.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Select cycle
  # --------------------------------------------------------------------------

  if (!is.null(cycle_id)) {

    if (!"cycle_id" %in% names(out)) {
      stop(
        "'cycle_id' is not available in 'swot_tibble'.",
        call. = FALSE
      )
    }

    out <- out |>
      dplyr::filter(
        .data$cycle_id %in% cycle_id
      )
  }


  # --------------------------------------------------------------------------
  # Select pass
  # --------------------------------------------------------------------------

  if (!is.null(pass_id)) {

    if (!"pass_id" %in% names(out)) {
      stop(
        "'pass_id' is not available in 'swot_tibble'.",
        call. = FALSE
      )
    }

    out <- out |>
      dplyr::filter(
        .data$pass_id %in% pass_id
      )
  }


  # --------------------------------------------------------------------------
  # Select observations around target time
  # --------------------------------------------------------------------------

  if (!is.null(time)) {

    time <- as.POSIXct(
      time,
      tz = "UTC"
    )

    time_difference <- abs(
      as.numeric(
        difftime(
          out$time,
          time,
          units = "mins"
        )
      )
    )

    out <- out[
      !is.na(time_difference) &
        time_difference <= time_tolerance,
      ,
      drop = FALSE
    ]
  }


  # --------------------------------------------------------------------------
  # Check observations
  # --------------------------------------------------------------------------

  if (nrow(out) == 0) {
    stop(
      "No SWOT observations match the requested profile.",
      call. = FALSE
    )
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


  if (nrow(out) == 0) {
    stop(
      "No observations remain after quality filtering.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Check requested variables
  # --------------------------------------------------------------------------

  if (!is.null(variable)) {

    variable <- unique(variable)

    missing_variables <- setdiff(
      variable,
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
  # Add standardized longitudinal distance
  # --------------------------------------------------------------------------

  if (distance_unit == "km") {

    out$distance <- out[[distance_col]] / 1000

  } else {

    out$distance <- out[[distance_col]]
  }


  # --------------------------------------------------------------------------
  # Select relevant variables
  # --------------------------------------------------------------------------

  if (!is.null(variable)) {

    uncertainty_cols <- paste0(
      variable,
      "_u"
    )

    uncertainty_cols <- intersect(
      uncertainty_cols,
      names(out)
    )

    metadata_cols <- intersect(
      c(
        "reach_id",
        "node_id",
        "time",
        "time_str",
        "cycle_id",
        "pass_id",
        q_col,
        "sword_version",
        distance_col,
        "distance"
      ),
      names(out)
    )

    keep_cols <- unique(
      c(
        metadata_cols,
        variable,
        uncertainty_cols
      )
    )

    out <- dplyr::select(
      out,
      dplyr::all_of(keep_cols)
    )
  }


  # --------------------------------------------------------------------------
  # Order longitudinally: upstream -> downstream
  # --------------------------------------------------------------------------

  out <- out |>
    dplyr::arrange(
      dplyr::desc(.data$distance)
    )


  # --------------------------------------------------------------------------
  # Add attributes
  # --------------------------------------------------------------------------

  attr(out, "feature_type") <- feature_type
  attr(out, "distance_unit") <- distance_unit
  attr(out, "distance_source") <- distance_col


  # --------------------------------------------------------------------------
  # Plot
  # --------------------------------------------------------------------------

  if (plot) {

    # ------------------------------------------------------------------------
    # Determine variable to plot
    # ------------------------------------------------------------------------

    if (is.null(plot_variable)) {

      # Automatically use variable if exactly one was requested
      if (!is.null(variable) && length(variable) == 1) {

        plot_variable <- variable

      } else {

        stop(
          "'plot_variable' must be supplied when plot = TRUE and ",
          "multiple variables are selected.",
          call. = FALSE
        )
      }
    }


    # ------------------------------------------------------------------------
    # Check plot variable
    # ------------------------------------------------------------------------

    if (length(plot_variable) != 1) {
      stop(
        "'plot_variable' must contain exactly one variable.",
        call. = FALSE
      )
    }

    if (!plot_variable %in% names(out)) {
      stop(
        "Plot variable '",
        plot_variable,
        "' was not found in the profile.",
        call. = FALSE
      )
    }

    if (!is.numeric(out[[plot_variable]])) {
      stop(
        "Plot variable '",
        plot_variable,
        "' must be numeric.",
        call. = FALSE
      )
    }


    # ------------------------------------------------------------------------
    # Axis labels
    # ------------------------------------------------------------------------

    y_label <- switch(
      plot_variable,
      wse = "Water surface elevation [m]",
      width = "River width [m]",
      slope = "Water surface slope",
      area_total = "Water surface area [m2]",
      plot_variable
    )

    x_label <- if (distance_unit == "km") {
      "Distance to outlet [km]"
    } else {
      "Distance to outlet [m]"
    }


    # ------------------------------------------------------------------------
    # Create plot
    # ------------------------------------------------------------------------

    p <- ggplot2::ggplot(
      out,
      ggplot2::aes(
        x = .data$distance,
        y = .data[[plot_variable]]
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_x_reverse() +
      ggplot2::labs(
        x = x_label,
        y = y_label
      ) +
      ggplot2::theme_minimal()

    return(p)
  }


  # --------------------------------------------------------------------------
  # Return data
  # --------------------------------------------------------------------------

  tibble::as_tibble(out)
}
