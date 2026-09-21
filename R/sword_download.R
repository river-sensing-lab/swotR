#' Download SWORD river network data
#'
#' @description
#' Downloads SWORD river network data as a foundation for working with
#' reaches and nodes of SWOT river observations.
#'
#' @param continent Character. Continent code to download, e.g. `"EU"`.
#' @param network Optional network ID used to filter the downloaded SWORD
#'   data to a specific river network. This works based on the "subnetork_id"
#' of the SWORD dataset
#' @param category Character. SWORD feature type to return. One of
#'   `"reaches"`, `"nodes"`, or `"both"`.
#' @param out_file Optional output file. For `category = "reaches"` or
#'   `"nodes"`, this specifies the output GeoPackage. For `category = "both"`,
#'   both layers are written to the same GeoPackage as layers `"reaches"`
#'   and `"nodes"`.
#' @param time_out Numeric. Download timeout in seconds. Default is 1200
#'   seconds (20 minutes).
#' @param aoi Optional `sf` polygon defining an area of interest used to
#'   spatially filter the SWORD data.
#' @param read Logical. Whether the downloaded data should be returned as
#'   `sf` objects. Default is `TRUE`.
#'
#' @author Florian Betz
#'
#' @references
#' Altenau et al. (2021): The Surface Water and Ocean Topography (SWOT)
#' Mission River Database (SWORD): A global river network for satellite
#' data products. Water Resources Research. https.doi.org/10.1029/2021WR030054
#'
#' @details
#' Please note that the download link is hard coded. If download errors
#' occur, check whether a newer SWORD release is available. Data is downloaded from
#' the SWORD Zenodo repository: https://doi.org/10.5281/zenodo.10013982
#'
#' @return
#' If `read = TRUE` and `category` is `"reaches"` or `"nodes"`, an `sf`
#' object is returned. If `category = "both"`, a named list containing
#' `reaches` and `nodes` as `sf` objects is returned.
#'
#' If `read = FALSE`, the function returns invisibly after writing the
#' requested data to `out_file`.
#'
#' @examples
#' \dontrun{
#' # Download SWORD reaches for Europe
#' reaches <- sword_download(
#'   continent = "EU",
#'   category = "reaches"
#' )
#'
#' # Download SWORD nodes for Europe
#' nodes <- sword_download(
#'   continent = "EU",
#'   category = "nodes"
#' )
#'
#' # Download reaches and nodes together
#' sword <- sword_download(
#'   continent = "EU",
#'   category = "both"
#' )
#'
#' # Access the individual layers
#' sword$reaches
#' sword$nodes
#'
#' # Write reaches and nodes to a GeoPackage
#' # Be careful: this creates a large file
#' sword_download(
#'   continent = "EU",
#'   category = "both",
#'   out_file = "sword_europe.gpkg",
#'   read = FALSE
#' )
#' }
#'
#' @importFrom utils download.file unzip
#' @export
#'
sword_download <- function(
    continent = "EU",
    network = NULL,
    category = c("reaches", "nodes", "both"),
    time_out = 1200,
    aoi = NULL,
    out_file = NULL,
    read = TRUE
) {

  # --------------------------------------------------------------------------
  # Check arguments
  # --------------------------------------------------------------------------

  category <- match.arg(category)

  if (!read && is.null(out_file)) {
    stop(
      "'out_file' must be provided when read = FALSE.",
      call. = FALSE
    )
  }

  if (!is.null(aoi)) {

    if (!inherits(aoi, "sf")) {
      stop(
        "'aoi' must be an sf object.",
        call. = FALSE
      )
    }

    if (is.na(sf::st_crs(aoi))) {
      stop(
        "'aoi' must have a defined coordinate reference system.",
        call. = FALSE
      )
    }
  }


  # --------------------------------------------------------------------------
  # Increase timeout
  # --------------------------------------------------------------------------

  old_timeout <- getOption("timeout")

  options(
    timeout = max(time_out, old_timeout)
  )

  on.exit(
    options(timeout = old_timeout),
    add = TRUE
  )


  # --------------------------------------------------------------------------
  # Create temporary workspace
  # --------------------------------------------------------------------------

  zip_file <- tempfile(fileext = ".zip")

  out_dir <- tempfile("sword_")
  dir.create(out_dir)

  on.exit(
    unlink(
      c(zip_file, out_dir),
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )


  # --------------------------------------------------------------------------
  # Download SWORD
  # --------------------------------------------------------------------------

  message("Downloading SWORD data...")

  utils::download.file(
    url = paste0(
      "https://zenodo.org/records/22259077/files/",
      "SWORD_v17c_gpkg.zip?download=1"
    ),
    destfile = zip_file,
    mode = "wb",
    quiet = FALSE
  )


  # --------------------------------------------------------------------------
  # Extract archive
  # --------------------------------------------------------------------------

  message("Extracting SWORD data...")

  utils::unzip(
    zipfile = zip_file,
    exdir = out_dir,
    overwrite = TRUE
  )


  # --------------------------------------------------------------------------
  # Find GeoPackage for requested continent
  # --------------------------------------------------------------------------

  files <- list.files(
    out_dir,
    pattern = "\\.gpkg$",
    full.names = TRUE,
    recursive = TRUE
  )

  continent_codes <- sub(
    "^sword_([^_]+)_.*$",
    "\\1",
    basename(files)
  )

  file <- files[
    continent_codes == toupper(continent)
  ]

  if (length(file) == 0) {
    stop(
      "No SWORD file found for continent '",
      continent,
      "'.",
      call. = FALSE
    )
  }

  if (length(file) > 1) {
    stop(
      "More than one SWORD file found for continent '",
      continent,
      "'.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Helper function for reading and filtering a SWORD layer
  # --------------------------------------------------------------------------

  read_sword_layer <- function(
    sword_layer,
    aoi_filter = NULL
  ) {

    message("Reading SWORD ", sword_layer, "...")

    x <- sf::st_read(
      dsn = file,
      layer = sword_layer,
      quiet = TRUE
    )


    # ------------------------------------------------------------------------
    # Check layer
    # ------------------------------------------------------------------------

    if (!inherits(x, "sf")) {
      stop(
        "SWORD layer '",
        sword_layer,
        "' could not be read as an sf object.",
        call. = FALSE
      )
    }


    # ------------------------------------------------------------------------
    # Filter by network ID
    # ------------------------------------------------------------------------

    if (!is.null(network)) {

      if (!"subnetwork_id" %in% names(x)) {
        stop(
          "The SWORD ",
          sword_layer,
          " layer does not contain a 'network' column.",
          call. = FALSE
        )
      }

      x <- x[
        as.character(x$subnetwork_id) %in% as.character(network),
        ,
        drop = FALSE
      ]

      if (nrow(x) == 0) {
        warning(
          "No ",
          sword_layer,
          " found for network ID ",
          paste(network, collapse = ", "),
          ".",
          call. = FALSE
        )
      }
    }


    # ------------------------------------------------------------------------
    # Spatial filter
    # ------------------------------------------------------------------------

    if (!is.null(aoi_filter)) {

      message("Spatially filtering ", sword_layer, "...")

      # Check AOI
      if (!inherits(aoi_filter, "sf")) {
        stop(
          "'aoi' must be an sf object.",
          call. = FALSE
        )
      }

      if (is.na(sf::st_crs(aoi_filter))) {
        stop(
          "'aoi' must have a defined coordinate reference system.",
          call. = FALSE
        )
      }

      if (is.na(sf::st_crs(x))) {
        stop(
          "The SWORD ",
          sword_layer,
          " layer has no defined coordinate reference system.",
          call. = FALSE
        )
      }


      # ----------------------------------------------------------------------
      # Transform AOI to SWORD CRS if necessary
      # ----------------------------------------------------------------------

      if (sf::st_crs(aoi_filter) != sf::st_crs(x)) {

        aoi_filter <- sf::st_transform(
          aoi_filter,
          sf::st_crs(x)
        )
      }


      # ----------------------------------------------------------------------
      # Identify features intersecting the AOI
      # ----------------------------------------------------------------------

      hits <- sf::st_intersects(
        x,
        aoi_filter
      )

      keep <- lengths(hits) > 0

      # Retain original SWORD geometries
      x <- x[
        keep,
        ,
        drop = FALSE
      ]


      # ----------------------------------------------------------------------
      # Check result
      # ----------------------------------------------------------------------

      if (nrow(x) == 0) {
        warning(
          "No ",
          sword_layer,
          " intersect the supplied AOI.",
          call. = FALSE
        )
      }
    }


    # ------------------------------------------------------------------------
    # Return layer
    # ------------------------------------------------------------------------

    x
  }


  # --------------------------------------------------------------------------
  # Read requested category/categories
  # --------------------------------------------------------------------------

  if (category == "both") {

    out <- list(

      reaches = read_sword_layer(
        sword_layer = "reaches",
        aoi_filter = aoi
      ),

      nodes = read_sword_layer(
        sword_layer = "nodes",
        aoi_filter = aoi
      )
    )

  } else {

    out <- read_sword_layer(
      sword_layer = category,
      aoi_filter = aoi
    )
  }


  # --------------------------------------------------------------------------
  # Write output
  # --------------------------------------------------------------------------

  if (!is.null(out_file)) {

    message("Writing SWORD data...")

    if (category == "both") {

      sf::st_write(
        out$reaches,
        out_file,
        layer = "reaches",
        delete_layer = TRUE,
        quiet = TRUE
      )

      sf::st_write(
        out$nodes,
        out_file,
        layer = "nodes",
        delete_layer = TRUE,
        quiet = TRUE
      )

    } else {

      sf::st_write(
        out,
        out_file,
        layer = category,
        delete_layer = TRUE,
        quiet = TRUE
      )
    }

    message(
      "SWORD network data stored as ",
      out_file
    )
  }


  # --------------------------------------------------------------------------
  # Return
  # --------------------------------------------------------------------------

  if (read) {
    return(out)
  }

  invisible(NULL)
}
