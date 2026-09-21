#' Summarize a SWORD river network
#'
#' @description
#' Provides a concise summary of a SWORD river network represented by either
#' reaches or nodes. The function automatically detects the feature type from
#' the presence of `reach_id` or `node_id`.
#'
#' For reach data, the summary includes the number of reaches, total river
#' length, river-name coverage, and, if available, information on Hack order.
#' For node data, the summary includes the number of nodes, the number of
#' represented reaches, and river-name coverage.
#'
#' The network can additionally be summarized by Hack order or river name.
#'
#' @param sword A SWORD `sf` object, data frame, or tibble containing either
#'   reaches or nodes.
#'
#' @param by Optional grouping variable. One of `NULL`, `"hack_order"`, or
#'   `"river_name"`. If `NULL`, an overall network summary is returned.
#'
#' @param length_col Optional name of the reach-length column. If `NULL`,
#'   the function attempts to identify a suitable length column automatically.
#'   This argument is only used for reach data.
#'
#' @param name_col Optional name of the river-name column. If `NULL`,
#'   the function attempts to identify a suitable name column automatically.
#'
#' @param length_unit Unit used for reporting river length. Either `"km"`
#'   or `"m"`. Default is `"km"`.
#'
#' @return
#' A tibble containing summary statistics for the supplied SWORD object.
#' If `by` is specified, one row is returned for each group.
#'
#' @examples
#' \dontrun{
#' reaches <- sword_download(
#'   ...,
#'   feature = "Reach"
#' )
#'
#' sword_summary(reaches)
#'
#' sword_summary(
#'   reaches,
#'   by = "river_name"
#' )
#'
#' reaches <- sword_topology(reaches)
#'
#' sword_summary(
#'   reaches,
#'   by = "hack_order"
#' )
#'
#' nodes <- sword_download(
#'   ...,
#'   feature = "Node"
#' )
#'
#' sword_summary(nodes)
#'
#' sword_summary(
#'   nodes,
#'   by = "river_name"
#' )
#' }
#'
#' @importFrom rlang .data
#' @export
sword_summary <- function(
    sword,
    by = NULL,
    length_col = NULL,
    name_col = NULL,
    length_unit = c("km", "m")
) {

  # --------------------------------------------------------------------------
  # Arguments
  # --------------------------------------------------------------------------

  length_unit <- match.arg(length_unit)

  allowed_by <- c(
    "hack_order",
    "river_name"
  )

  if (!is.null(by) && !by %in% allowed_by) {
    stop(
      "'by' must be NULL or one of: ",
      paste(allowed_by, collapse = ", "),
      ".",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Remove geometry
  # --------------------------------------------------------------------------

  if (inherits(sword, "sf")) {
    sword <- sf::st_drop_geometry(sword)
  }


  # --------------------------------------------------------------------------
  # Recognize reach- or node-based input
  # --------------------------------------------------------------------------

  if ("node_id" %in% names(sword)) {

    feature_type <- "Node"

  } else if ("reach_id" %in% names(sword)) {

    feature_type <- "Reach"

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

    if (length(name_matches) > 0) {
      name_col <- name_matches[1]
    }

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
  # Prepare river names
  # --------------------------------------------------------------------------

  if (!is.null(name_col)) {

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
        "NODATA"
      )

    sword$.river_name <- river_name

    sword$.river_name[!named] <- "<unnamed>"

  } else {

    named <- rep(
      FALSE,
      nrow(sword)
    )

    sword$.river_name <- rep(
      "<unnamed>",
      nrow(sword)
    )
  }

  sword$.named <- named


  # ==========================================================================
  # NODE DATA
  # ==========================================================================

  if (feature_type == "Node") {

    # ------------------------------------------------------------------------
    # Basic counts
    # ------------------------------------------------------------------------

    n_nodes <- dplyr::n_distinct(
      sword$node_id,
      na.rm = TRUE
    )

    n_reaches <- if ("reach_id" %in% names(sword)) {

      dplyr::n_distinct(
        sword$reach_id,
        na.rm = TRUE
      )

    } else {

      NA_integer_
    }


    # ------------------------------------------------------------------------
    # Overall node summary
    # ------------------------------------------------------------------------

    if (is.null(by)) {

      n_named_nodes <- dplyr::n_distinct(
        sword$node_id[sword$.named],
        na.rm = TRUE
      )

      n_river_names <- dplyr::n_distinct(
        sword$.river_name[sword$.named],
        na.rm = TRUE
      )


      # ----------------------------------------------------------------------
      # Topology information
      # ----------------------------------------------------------------------

      topology_available <- "hack_order" %in%
        names(sword)

      if (topology_available) {

        hack_values <- sword$hack_order[
          !is.na(sword$hack_order)
        ]

        n_hack_orders <- dplyr::n_distinct(
          hack_values
        )

        max_hack_order <- if (length(hack_values) > 0) {
          max(hack_values)
        } else {
          NA_integer_
        }

      } else {

        n_hack_orders <- NA_integer_
        max_hack_order <- NA_integer_
      }


      # ----------------------------------------------------------------------
      # Return
      # ----------------------------------------------------------------------

      return(
        tibble::tibble(
          feature_type = feature_type,
          n_nodes = n_nodes,
          n_reaches = n_reaches,
          n_named_nodes = n_named_nodes,
          pct_named_nodes = if (n_nodes > 0) {
            100 * n_named_nodes / n_nodes
          } else {
            NA_real_
          },
          n_river_names = n_river_names,
          topology_available = topology_available,
          n_hack_orders = n_hack_orders,
          max_hack_order = max_hack_order
        )
      )
    }


    # ------------------------------------------------------------------------
    # Node summary by river name
    # ------------------------------------------------------------------------

    if (by == "river_name") {

      out <- sword |>
        dplyr::group_by(
          river_name = .data$.river_name
        ) |>
        dplyr::summarise(
          n_nodes = dplyr::n_distinct(
            .data$node_id
          ),
          n_reaches = if ("reach_id" %in% names(sword)) {
            dplyr::n_distinct(
              .data$reach_id
            )
          } else {
            NA_integer_
          },
          .groups = "drop"
        ) |>
        dplyr::arrange(
          dplyr::desc(.data$n_nodes)
        )

      return(
        tibble::as_tibble(out)
      )
    }


    # ------------------------------------------------------------------------
    # Node summary by Hack order
    # ------------------------------------------------------------------------

    if (by == "hack_order") {

      if (!"hack_order" %in% names(sword)) {
        stop(
          "'hack_order' is not available in the SWORD nodes. ",
          "Run sword_topology() or attach reach topology before ",
          "requesting a summary by Hack order.",
          call. = FALSE
        )
      }

      out <- sword |>
        dplyr::filter(
          !is.na(.data$hack_order)
        ) |>
        dplyr::group_by(
          .data$hack_order
        ) |>
        dplyr::summarise(
          n_nodes = dplyr::n_distinct(
            .data$node_id
          ),
          n_reaches = if ("reach_id" %in% names(sword)) {
            dplyr::n_distinct(
              .data$reach_id
            )
          } else {
            NA_integer_
          },
          .groups = "drop"
        ) |>
        dplyr::arrange(
          .data$hack_order
        )

      return(
        tibble::as_tibble(out)
      )
    }
  }


  # ==========================================================================
  # REACH DATA
  # ==========================================================================

  if (feature_type == "Reach") {

    # ------------------------------------------------------------------------
    # Identify reach-length column
    # ------------------------------------------------------------------------

    if (is.null(length_col)) {

      length_candidates <- c(
        "reach_length",
        "reach_len",
        "length",
        "len"
      )

      length_matches <- length_candidates[
        length_candidates %in% names(sword)
      ]

      if (length(length_matches) == 0) {
        stop(
          "No reach-length column could be identified. ",
          "Supply its name using 'length_col'.",
          call. = FALSE
        )
      }

      length_col <- length_matches[1]

    } else {

      if (!length_col %in% names(sword)) {
        stop(
          "Length column '",
          length_col,
          "' was not found in 'sword'.",
          call. = FALSE
        )
      }
    }


    # ------------------------------------------------------------------------
    # Prepare reach length
    # ------------------------------------------------------------------------

    sword$.river_length <- sword[[length_col]]

    if (length_unit == "km") {

      sword$.river_length <-
        sword$.river_length / 1000

      length_name <- "river_length_km"

    } else {

      length_name <- "river_length_m"
    }


    # ------------------------------------------------------------------------
    # Overall reach summary
    # ------------------------------------------------------------------------

    if (is.null(by)) {

      n_reaches <- dplyr::n_distinct(
        sword$reach_id,
        na.rm = TRUE
      )

      n_named_reaches <- dplyr::n_distinct(
        sword$reach_id[sword$.named],
        na.rm = TRUE
      )

      n_river_names <- dplyr::n_distinct(
        sword$.river_name[sword$.named],
        na.rm = TRUE
      )


      # ----------------------------------------------------------------------
      # Topology information
      # ----------------------------------------------------------------------

      topology_available <- "hack_order" %in%
        names(sword)

      if (topology_available) {

        hack_values <- sword$hack_order[
          !is.na(sword$hack_order)
        ]

        n_hack_orders <- dplyr::n_distinct(
          hack_values
        )

        max_hack_order <- if (length(hack_values) > 0) {
          max(hack_values)
        } else {
          NA_integer_
        }

      } else {

        n_hack_orders <- NA_integer_
        max_hack_order <- NA_integer_
      }


      # ----------------------------------------------------------------------
      # Create summary
      # ----------------------------------------------------------------------

      out <- tibble::tibble(
        feature_type = feature_type,
        n_reaches = n_reaches,
        river_length = sum(
          sword$.river_length,
          na.rm = TRUE
        ),
        n_named_reaches = n_named_reaches,
        pct_named_reaches = if (n_reaches > 0) {
          100 * n_named_reaches / n_reaches
        } else {
          NA_real_
        },
        n_river_names = n_river_names,
        topology_available = topology_available,
        n_hack_orders = n_hack_orders,
        max_hack_order = max_hack_order
      )

      names(out)[
        names(out) == "river_length"
      ] <- length_name

      return(out)
    }


    # ------------------------------------------------------------------------
    # Reach summary by river name
    # ------------------------------------------------------------------------

    if (by == "river_name") {

      out <- sword |>
        dplyr::group_by(
          river_name = .data$.river_name
        ) |>
        dplyr::summarise(
          n_reaches = dplyr::n_distinct(
            .data$reach_id
          ),
          river_length = sum(
            .data$.river_length,
            na.rm = TRUE
          ),
          .groups = "drop"
        ) |>
        dplyr::arrange(
          dplyr::desc(.data$river_length)
        )

      names(out)[
        names(out) == "river_length"
      ] <- length_name

      return(
        tibble::as_tibble(out)
      )
    }


    # ------------------------------------------------------------------------
    # Reach summary by Hack order
    # ------------------------------------------------------------------------

    if (by == "hack_order") {

      if (!"hack_order" %in% names(sword)) {
        stop(
          "'hack_order' is not available in the SWORD reaches. ",
          "Run sword_topology() before requesting a summary by Hack order.",
          call. = FALSE
        )
      }

      out <- sword |>
        dplyr::filter(
          !is.na(.data$hack_order)
        ) |>
        dplyr::group_by(
          .data$hack_order
        ) |>
        dplyr::summarise(
          n_reaches = dplyr::n_distinct(
            .data$reach_id
          ),
          river_length = sum(
            .data$.river_length,
            na.rm = TRUE
          ),
          n_named_reaches = dplyr::n_distinct(
            .data$reach_id[.data$.named]
          ),
          pct_named_reaches =
            100 *
            dplyr::n_distinct(
              .data$reach_id[.data$.named]
            ) /
            dplyr::n_distinct(
              .data$reach_id
            ),
          .groups = "drop"
        ) |>
        dplyr::arrange(
          .data$hack_order
        )

      names(out)[
        names(out) == "river_length"
      ] <- length_name

      return(
        tibble::as_tibble(out)
      )
    }
  }
}
