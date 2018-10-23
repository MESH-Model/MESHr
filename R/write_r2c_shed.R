#' Write MESH watershed data to r2c file
#'
#' @param basin Required. The values defining the basin parameters, as read in by 
#' \code{read_r2c_shed}. Can either be and array or a raster brick.
#' @param header Required. The header lines from the basin r2c file, as read in by 
#' \code{read_r2c_shed}
#' @param r2cFile Required. The file to be written.
#'
#' @return If successful, returns \code{TRUE}. If unsucessful, returns 
#' \code{FALSE}
#' @author Kevin Shook
#' @seealso \code{\link{read_r2c_shed}}
#' @export
#'
#' @examples \dontrun{
#' # read in basin
#' shed <- read_r2c_shed("MESH_drainage_database.r2c", values_only = FALSE, 
#' as_rasters = FALSE)
#' # write as another file
#' write_r2c_shed(shed$basin, shed$header_lines, "new_basin.r2c")
#' }
write_r2c_shed <- function(basin, header, r2cFile = "") {
  # check values
  if (class(basin[[1]]) == "RasterBrick") {
    basin_type <- "raster_brick"
  } else {
    basin_type <- "array"
  }
  
  if (length(header) < 1) {
    cat("Error: header data is missing\n")
    return(FALSE)
  }
  
  if (r2cFile == "") {
    cat("Error: basin file is missing\n")
    return(FALSE)
  }
  
  # find eol to use
  eol <- win.eol()
  
  # write header
  header <- c(header, ":EndHeader")
  
  zz <- file(r2cFile, "w")
  result <- writeLines(header, con = zz, sep = eol)

  # now write output
  numrows <- dim(basin)[1]
  numcols <- dim(basin)[2]
  numlayers <- dim(basin)[3]
  
  
  for (i in 1:numlayers) {
    if (basin_type == "array")
      layer <- basin[,,i]
    else
      layer <- raster::as.matrix(basin[[i]])
    
    # now flip the layer vertically
    layer_flipped <- apply(layer, 2, rev)
    
  # write to file  
    for (j in 1:numrows) {
      formatted <- formatC(layer_flipped[j,], format = "f",
                           digits = 8, width = 19)
      all_formatted <- paste(formatted, collapse = "")
      result <- writeLines(all_formatted, con = zz,  sep = eol)
    }
  }
  
  close(zz)
  return(TRUE)
}