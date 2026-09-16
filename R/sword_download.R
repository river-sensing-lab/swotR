#' Download SWORD river network data
#' @description Downloads SWORD river network data as foundation to work with the reaches and nodes of the SWOT river observations
#' @param continent which continent to download, see also
#' @param network if known, you can filter directly by the network ID to e.g. obtain the network of a specific basin
#' @param category what to get from the SWORD data, reaches or nodes. Can be "reaches" or "nodes"
#' @param out_file output file of the selected reaches or nodes
#' @param time_out increase timeout of 60 seconds to something more meaningful for GB scale data;
#' default is 20 minutes what should be sufficient even with slower internet connections.
#' @param aoi sf object with your area of interest as polygon
#' @param read whether the downloaded data should be directly read as sf object or not; default is TRUE
#' @author Florian Betz
#' @references - Altenau et al. (2021): The Surface Water and Ocean Topography (SWOT) Mission River Database (SWORD):
#' A global river network for satellite data products". Water Resources Research. https://doi.org/10.1029/2021WR030054
#' @details Please note that the download links are hard coded, so consider looking in the source code if errors arise
#' @return if read=TRUE, the function returns an sf object of the downloaded network, otherwise it just downloads the data and returns a
#' message with the location where the data is stored
#' @importFrom utils download.file
#' @importFrom utils unzip
#' @export sword_download
#'

sword_download<- function(continent="EU", network=NULL, category="reaches", time_out=1200, aoi=NULL ,out_file,read=TRUE){

  #Increase default timeout of R (or keep higher default if previously set by the user)
  options(timeout = max(time_out, getOption("timeout")))

  #Set download temp files and let it delete after the exit from the function
  zip_file<-tempfile(fileext = ".zip")

  message("Downloading data...")

  #Download the latest SWORD data from Zenodo (https://doi.org/10.5281/zenodo.3898569)
  utils::download.file("https://zenodo.org/records/22259077/files/SWORD_v17c_gpkg.zip?download=1",
                destfile = zip_file, mode="wb",quiet=FALSE)

  message("unzipping...")

  out_dir<-dir.create(tempfile("sword_"))

  #Unzip the file; set invisible to keep console clean
  files<-utils::unzip(zipfile = zip_file,exdir = out_dir,overwrite = TRUE)

  #Build the proper names for grabbing the proper continents
  file<-file <- files[grepl(paste0("^sword_", continent, "_"), basename(files))]

  #Get final file for the continent and category
  sword_file<-sf::st_read(file,layer=category)

  #Spatially filter the data by intersection with a given aoi
  if (!is.null(aoi)) {
    message("Filtering data")
    sword_intersected<-sf::st_intersection(sword_file,sf::st_union(sf::st_geometry(aoi)))
    out<-sword_intersected
  }

  #Writing output file
  sf::st_write(out,out_file)

  message(paste0("SWORD network data stored as ", out_file))

  #clean up temporary files
  rm(sword_file)
  rm(out)
  unlink(zip_file)
  unlink(out_dir,recursive = TRUE,force=TRUE)

  #Return also sf object along with the output file message
  if (read) {sf::st_read(out_file)}

}
