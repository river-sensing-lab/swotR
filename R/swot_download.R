#' Download SWOT RiverSP time series
#'
#' @description Downloads SWOT RiverSP time series for river nodes or reaches using
#' NASA PO.DAAC's Hydrocron API. Features can either be supplied directly through their SWORD node/reach IDs
#' or through an sf object containing a `node_id` or `reach_id` column.
#' @param network Optional sf object containing SWORD nodes or reaches.
#'   For `type = "Node"` the object must contain a `node_id` column.
#'   For `type = "Reach"` the object must contain a `reach_id` column.
#' @param id Optional node or reach ID, or vector of IDs. Can be supplied
#'   instead of `network`.
#' @param type Character. Either `"Node"` or `"Reach"`.
#' @param start_time Start of the requested time period. Can be supplied as
#'   `"YYYY-MM-DD"`, a full ISO timestamp, Date, or POSIXct.
#' @param end_time End of the requested time period.
#' @param time_out HTTP timeout in seconds. Default is 1200.
#' @param quality Quality filtering. One of `"all"`, `"good"`,
#'   `"good_suspect"`, or `"usable"`.
#' @param out_format Output format. Either `"csv"` or `"geojson"`.
#' @param fields Optional character vector of Hydrocron variables.
#'   If NULL, a default set of variables is downloaded.
#' @param collection_name Hydrocron SWOT collection. Defaults to
#'   `"SWOT_L2_HR_RiverSP_D"`.
#' @param quiet Logical. If FALSE, progress messages are printed.
#'
#' @return A tibble for `out_format = "csv"` or an sf object for
#' `out_format = "geojson"`. Returns an empty tibble/sf object if no
#' observations are found.
#'
#' @author Florian Betz
#' @export swot_download
#'

swot_download <- function(network = NULL,
                          id = NULL,
                          type = "Node",
                          start_time,
                          end_time,
                          time_out = 1200,
                          quality = "all",
                          out_format = "csv",
                          fields = NULL,
                          collection_name = "SWOT_L2_HR_RiverSP_D",
                          quiet = FALSE) {

  # --------------------------------------------------------------------------
  # Argument checks
  # --------------------------------------------------------------------------

  type <- match.arg(type, c("Node", "Reach"))

  out_format <- match.arg(
    out_format,
    c("csv", "geojson")
  )

  quality <- match.arg(
    quality,
    c("all", "good", "good_suspect", "usable")
  )

  if (is.null(network) && is.null(id)) {
    stop(
      "Either 'network' or 'id' must be supplied.",
      call. = FALSE
    )
  }

  if (!is.null(network) && !is.null(id)) {
    stop(
      "Supply either 'network' or 'id', not both.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Determine feature IDs
  # --------------------------------------------------------------------------

  id_field <- if (type == "Node") {
    "node_id"
  } else {
    "reach_id"
  }

  if (!is.null(network)) {

    if (!inherits(network, "sf")) {
      stop(
        "'network' must be an sf object.",
        call. = FALSE
      )
    }

    if (!id_field %in% names(network)) {
      stop(
        "'network' must contain a '",
        id_field,
        "' column.",
        call. = FALSE
      )
    }

    ids <- network[[id_field]]

  } else {

    ids <- id
  }

  # Important for large SWORD IDs:
  # keep IDs as character strings.
  ids <- as.character(ids)

  ids <- unique(
    ids[!is.na(ids) & nzchar(ids)]
  )

  if (length(ids) == 0) {
    stop(
      "No valid SWORD feature IDs found.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Hydrocron time formatter
  # --------------------------------------------------------------------------

  format_hydrocron_time <- function(x, end = FALSE) {

    # Date input
    if (inherits(x, "Date")) {

      date_string <- format(
        x,
        "%Y-%m-%d"
      )

      time_part <- if (end) {
        "23:59:59"
      } else {
        "00:00:00"
      }

      return(
        paste0(
          date_string,
          "T",
          time_part,
          "Z"
        )
      )
    }


    # Character input
    if (is.character(x)) {

      # Simple YYYY-MM-DD
      if (
        length(x) == 1 &&
        grepl(
          "^\\d{4}-\\d{2}-\\d{2}$",
          x
        )
      ) {

        time_part <- if (end) {
          "23:59:59"
        } else {
          "00:00:00"
        }

        return(
          paste0(
            x,
            "T",
            time_part,
            "Z"
          )
        )
      }


      # Already correctly formatted ISO timestamp
      if (
        length(x) == 1 &&
        grepl(
          "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$",
          x
        )
      ) {
        return(x)
      }


      # Try other date-time formats
      x <- as.POSIXct(
        x,
        tz = "UTC",
        tryFormats = c(
          "%Y-%m-%d %H:%M:%S",
          "%Y-%m-%dT%H:%M:%S"
        )
      )
    }


    # POSIXct / POSIXlt
    if (inherits(x, c("POSIXct", "POSIXlt"))) {

      return(
        format(
          x,
          "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        )
      )
    }

    stop(
      "Could not interpret date/time input.",
      call. = FALSE
    )
  }


  start_time <- format_hydrocron_time(
    start_time,
    end = FALSE
  )

  end_time <- format_hydrocron_time(
    end_time,
    end = TRUE
  )


  # --------------------------------------------------------------------------
  # Default fields
  # --------------------------------------------------------------------------

  if (is.null(fields)) {

    if (type == "Node") {

      fields <- c(
        "node_id",
        "reach_id",
        "time_str",
        "lat",
        "lon",
        "wse",
        "wse_u",
        "width",
        "width_u",
        "area_total",
        "area_tot_u",
        "node_q",
        "cycle_id",
        "pass_id",
        "sword_version"
      )

    } else {

      fields <- c(
        "reach_id",
        "time_str",
        "p_lat",
        "p_lon",
        "wse",
        "wse_u",
        "slope",
        "slope_u",
        "width",
        "width_u",
        "area_total",
        "area_tot_u",
        "reach_q",
        "cycle_id",
        "pass_id",
        "sword_version"
      )
    }
  }

  fields <- paste(
    fields,
    collapse = ","
  )


  # --------------------------------------------------------------------------
  # Hydrocron URL
  # --------------------------------------------------------------------------

  hydrocron_url <-
    "https://soto.podaac.earthdatacloud.nasa.gov/hydrocron/v1/timeseries"


  # --------------------------------------------------------------------------
  # Download one feature
  # --------------------------------------------------------------------------

  download_one <- function(feature_id) {

    if (!quiet) {
      message(
        "Downloading ",
        type,
        " ",
        feature_id
      )
    }


    req <- httr2::request(
      hydrocron_url
    ) |>
      httr2::req_url_query(
        feature = type,
        feature_id = feature_id,
        collection_name = collection_name,
        start_time = start_time,
        end_time = end_time,
        output = out_format,
        fields = fields
      ) |>
      httr2::req_timeout(
        time_out
      ) |>
      httr2::req_error(
        is_error = function(resp) FALSE
      )


    # ------------------------------------------------------------------------
    # Perform request
    # ------------------------------------------------------------------------

    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) {

        warning(
          "Request failed for ",
          type,
          " ",
          feature_id,
          ": ",
          conditionMessage(e),
          call. = FALSE
        )

        return(NULL)
      }
    )

    if (is.null(resp)) {
      return(NULL)
    }


    # ------------------------------------------------------------------------
    # HTTP status handling
    # ------------------------------------------------------------------------

    status <- httr2::resp_status(resp)

    txt <- httr2::resp_body_string(resp)

    if (status >= 400) {

      no_data <- grepl(
        "Results with the specified Feature ID.*were not found",
        txt,
        ignore.case = TRUE
      )

      if (no_data) {

        if (!quiet) {
          message(
            "No observations found for ",
            type,
            " ",
            feature_id,
            " in the requested period."
          )
        }

        return(NULL)
      }

      warning(
        "Hydrocron request failed for ",
        type,
        " ",
        feature_id,
        " (HTTP ",
        status,
        "): ",
        txt,
        call. = FALSE
      )

      return(NULL)
    }


    # ------------------------------------------------------------------------
    # Parse JSON wrapper
    # ------------------------------------------------------------------------

    js <- jsonlite::fromJSON(
      txt,
      simplifyVector = FALSE
    )

    if (is.null(js$results)) {
      return(NULL)
    }


    # ------------------------------------------------------------------------
    # CSV
    # ------------------------------------------------------------------------

    if (out_format == "csv") {

      csv_txt <- js$results$csv

      if (
        is.null(csv_txt) ||
        length(csv_txt) == 0 ||
        !nzchar(csv_txt)
      ) {
        return(NULL)
      }


      # Important:
      # Hydrocron responses are read separately for each feature.
      # readr would otherwise guess column types independently, which can
      # result in e.g. time_str being character for one reach and POSIXct
      # for another. Explicitly fixing the structural columns avoids this.
      dat <- readr::read_csv(
        I(csv_txt),
        col_types = readr::cols(
          node_id = readr::col_character(),
          reach_id = readr::col_character(),
          time_str = readr::col_character(),
          sword_version = readr::col_character(),
          .default = readr::col_guess()
        ),
        show_col_types = FALSE
      )

      return(dat)
    }


    # ------------------------------------------------------------------------
    # GeoJSON
    # ------------------------------------------------------------------------

    geojson <- js$results$geojson

    if (
      is.null(geojson) ||
      is.null(geojson$features) ||
      length(geojson$features) == 0
    ) {
      return(NULL)
    }

    geojson_txt <- jsonlite::toJSON(
      geojson,
      auto_unbox = TRUE,
      null = "null"
    )

    tmp_geojson <- tempfile(
      fileext = ".geojson"
    )

    writeLines(
      geojson_txt,
      tmp_geojson
    )

    on.exit(
      unlink(tmp_geojson),
      add = TRUE
    )

    dat <- sf::st_read(
      tmp_geojson,
      quiet = TRUE
    )

    dat
  }


  # --------------------------------------------------------------------------
  # Download all features
  # --------------------------------------------------------------------------

  result_list <- lapply(
    ids,
    download_one
  )

  result_list <- Filter(
    Negate(is.null),
    result_list
  )


  # --------------------------------------------------------------------------
  # No results
  # --------------------------------------------------------------------------

  if (length(result_list) == 0) {

    if (!quiet) {
      message(
        "No SWOT observations found."
      )
    }

    if (out_format == "csv") {
      return(
        tibble::tibble()
      )
    }

    return(
      sf::st_sf(
        geometry = sf::st_sfc(
          crs = 4326
        )
      )
    )
  }


  # --------------------------------------------------------------------------
  # Combine results
  # --------------------------------------------------------------------------

  if (out_format == "csv") {

    out <- dplyr::bind_rows(
      result_list
    )

  } else {

    out <- do.call(
      rbind,
      result_list
    )
  }


  # --------------------------------------------------------------------------
  # Parse observation time
  # --------------------------------------------------------------------------

  if ("time_str" %in% names(out)) {

    out$time <- as.POSIXct(
      out$time_str,
      format = "%Y-%m-%dT%H:%M",
      tz = "UTC"
    )
  }


  # --------------------------------------------------------------------------
  # Quality filtering
  # --------------------------------------------------------------------------

  q_field <- if (type == "Node") {
    "node_q"
  } else {
    "reach_q"
  }

  if (
    quality != "all" &&
    q_field %in% names(out)
  ) {

    allowed_q <- switch(
      quality,
      good = 0,
      good_suspect = c(0, 1),
      usable = c(0, 1, 2)
    )

    out <- out[
      out[[q_field]] %in% allowed_q,
    ]
  }


  # --------------------------------------------------------------------------
  # Order result
  # --------------------------------------------------------------------------

  if ("time" %in% names(out)) {

    id_col <- if (type == "Node") {
      "node_id"
    } else {
      "reach_id"
    }

    if (id_col %in% names(out)) {

      out <- out[
        order(
          out[[id_col]],
          out$time
        ),
      ]
    }
  }


  # --------------------------------------------------------------------------
  # Return
  # --------------------------------------------------------------------------

  out
}
