#' Computing network topology for the SWORD network data
#' @description classifies the mainstem and gives unique identifiers to each tributary
#' @param network the river network from the SWORD database; this function requires v17c of the SWORD database
#' @author Florian Betz
#' @return returns an sf object with the
#' @export sword_topology
#'

sword_topology <- function(network) {

  required <- c(
    "reach_id",
    "main_path_id",
    "rch_id_dn_main",
    "hydro_dist_out"
  )

  missing <- dplyr::setdiff(required, names(network))

  if (length(missing) > 0) {
    stop(
      "SWORD v17c variables missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # --------------------------------------------------------
  # Convert IDs to character
  # --------------------------------------------------------

  network$reach_id <- as.character(network$reach_id)
  network$main_path_id <- as.character(network$main_path_id)
  network$rch_id_dn_main <- as.character(network$rch_id_dn_main)

  # Convenience variables
  network$branch_id <- network$main_path_id
  network$longitudinal_distance <- network$hydro_dist_out

  net <- sf::st_drop_geometry(network)

  # --------------------------------------------------------
  # Lookup: reach_id -> main_path_id
  # --------------------------------------------------------

  reach_to_path <- stats::setNames(
    net$main_path_id,
    net$reach_id
  )

  paths <- unique(net$main_path_id)

  paths <- paths[
    !is.na(paths) &
      nzchar(paths)
  ]


  # ========================================================
  # Determine downstream main_path_id for every path
  # ========================================================

  downstream_path <- stats::setNames(
    rep(NA_character_, length(paths)),
    paths
  )

  for (path in paths) {

    ii <- which(net$main_path_id == path)

    # Main downstream reach for reaches belonging to this path
    dn_reaches <- net$rch_id_dn_main[ii]

    dn_reaches <- dn_reaches[
      !is.na(dn_reaches) &
        nzchar(dn_reaches) &
        dn_reaches != "0" &
        dn_reaches != "-9999"
    ]

    if (length(dn_reaches) == 0) {
      next
    }

    # Translate downstream reach IDs to path IDs
    dn_paths <- unname(
      reach_to_path[dn_reaches]
    )

    dn_paths <- dn_paths[
      !is.na(dn_paths) &
        dn_paths != path
    ]

    # If a reach leaves the current main_path, this identifies
    # the receiving downstream path.
    if (length(dn_paths) > 0) {

      dn_paths <- unique(dn_paths)

      if (length(dn_paths) > 1) {
        warning(
          "Path ", path,
          " appears to connect to multiple downstream paths: ",
          paste(dn_paths, collapse = ", "),
          call. = FALSE
        )
      }

      downstream_path[path] <- dn_paths[1]
    }
  }


  # ========================================================
  # Identify terminal/mainstem path(s)
  # ========================================================

  # A terminal path is one whose downstream continuation is
  # outside the supplied network or does not enter another path.
  terminal_paths <- names(downstream_path)[
    is.na(downstream_path)
  ]

  if (length(terminal_paths) == 0) {
    stop(
      "Could not identify a terminal main path.",
      call. = FALSE
    )
  }


  # ========================================================
  # Calculate Hack order
  # ========================================================

  hack <- stats::setNames(
    rep(NA_integer_, length(paths)),
    paths
  )

  # Terminal/mainstem paths are order 1
  hack[terminal_paths] <- 1L


  get_hack_order <- function(path,
                             visited = character()) {

    # Already assigned
    if (!is.na(hack[path])) {
      return(hack[path])
    }

    # Protect against cycles
    if (path %in% visited) {
      warning(
        "Circular path topology detected at main_path_id ",
        path,
        call. = FALSE
      )
      return(NA_integer_)
    }

    dn <- downstream_path[path]

    # Downstream path absent from subset
    if (is.na(dn) || !dn %in% names(hack)) {
      return(NA_integer_)
    }

    dn_order <- get_hack_order(
      dn,
      visited = c(visited, path)
    )

    if (is.na(dn_order)) {
      return(NA_integer_)
    }

    hack[path] <<- dn_order + 1L

    hack[path]
  }


  for (path in paths) {
    get_hack_order(path)
  }


  # --------------------------------------------------------
  # Join to reaches
  # --------------------------------------------------------

  network$hack_order <- unname(
    hack[network$main_path_id]
  )

  # Mainstem according to our Hack hierarchy
  network$is_mainstem_hack <- network$hack_order == 1L

  return(network)
}
