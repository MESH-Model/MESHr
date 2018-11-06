#' Reads r2c file of a MESH watershed
#' 
#' @description This function reads in a file containing the layers which
#' define a MESH watershed. To read in a file of timeseries, use the function
#' \code{r2c2raster}.
#' @param r2cFile Required. Name of \code{r2c} file.
#' @param values_only Optional. If \code{TRUE} (the default), then only the values
#' are returned, either as a raster brick (useful for plotting) or as a 3D array 
#' (useful for analysis). If \code{FALSE}, then a \emph{list} will be returned, 
#' containing the 1) the data, 2) the metadata (the variable names, types and 
#' units) are returned for each layer, and 3) the r2c file header lines.
#' @param as_rasters Optional. If \code{TRUE}, the layers will be returned as 
#' as raster brick. If \code{FALSE}, they will be returned as an array.
#'
#' @return Returns eiher an array or a raster brick of values, and optionally,
#' the meta data and file header.
#' @author Kevin Shook
#' @seealso \code{\link{read_r2c_raster}}
#' @export
#'
#' @examples \dontrun{
#' # read in basin as a raster brick
#' basin <-  r2c2basin("MESH_drainage_database.r2c")
#' # read in as an array
#' basin_array <- r2c2basin("MESH_drainage_database.r2c", as_rasters = FALSE)
#' # get meta data as well
#' basin_array <- r2c2basin("MESH_drainage_database.r2c", values_only = FALSE, 
#' as_rasters = FALSE)
#' }
#' 
read_r2c_shed <- function(r2cFile = "", values_only = TRUE, as_rasters = TRUE) {
  
  if (r2cFile == "") {
    cat("Error: missing MESH basin file\n")
    return(FALSE)
  }
  
  # read in file
  con <- file(r2cFile, "r", blocking = FALSE, encoding = "ISO_8859-2")
  r2c <- readLines(con)
  close(con)
  
  # check to make sure it isn't a timeseries file
  name_present <- grep(":Name", r2c, fixed = TRUE)
  
  # if no frames, terminate as this is not a time series file
  if (length(name_present) > 0) {
    cat("Error: not a MESH basin file\n")
    return(FALSE)
  }
  
  # parse file
  # first get header info
  data_type <- findRecord(r2c, "# DataType")
  
  # check to make sure it's a 2D file
  if (data_type != "2D Rect Cell") {
    cat("Error: not a MESH basin file\n")
    return(FALSE)
  }
  
  projection <- findRecord(r2c, ":Projection")
  ellipsoid <- findRecord(r2c, ":Ellipsoid")
  xOrigin <- as.numeric(findRecord(r2c, ":xOrigin"))
  yOrigin <- as.numeric(findRecord(r2c, ":yOrigin"))  
  xCount <- as.numeric(findRecord(r2c, ":xCount"))  
  yCount <- as.numeric(findRecord(r2c, ":yCount"))   
  xDelta <- as.numeric(findRecord(r2c, ":xDelta"))  
  yDelta <- as.numeric(findRecord(r2c, ":yDelta")) 
  
  # get attribute names and types/units
  attribute_name_locs <- grep(":AttributeName", r2c, fixed = TRUE)
  attribute_count <- length(attribute_name_locs)
  attribute_type_locs <- attribute_name_locs + 1
  attribute_name_lines <- r2c[attribute_name_locs]
  attribute_type_lines <- r2c[attribute_type_locs]
  
  attribute_names <- stringr::str_split_fixed(attribute_name_lines, stringr::fixed(" "), n = 3)[,3]
  attribute_type_names <- stringr::str_split_fixed(attribute_type_lines, stringr::fixed(" "), n = 3)[,1]
  attribute_type_vals <- stringr::str_split_fixed(attribute_type_lines, stringr::fixed(" "), n = 3)[,3]
  
  # data is following header
  end_header <-  grep(":EndHeader", r2c, fixed = TRUE)
  header_lines <- r2c[1:(end_header - 1)]
  
  
  # parse all nums
  data_vals <- read.table(r2cFile, skip = end_header, header = FALSE)

  # now force into an array
   output_vals <- array(0, c(yCount, xCount, attribute_count))

  for (i in 1:attribute_count) {
    start_row <- 1 + (i - 1) * yCount
    end_row <- start_row + (yCount - 1)
    layer_vals <- data_vals[start_row:end_row,]
    
    # flip vals vertically
    layer_vals_flipped <- apply(layer_vals, 2, rev)
    output_vals[,, i] <- layer_vals_flipped
  }
  
  # add information if requested
  if (values_only & !as_rasters) {
    return(output_vals)
  }
   
  if (as_rasters) {
    # convert matrix to raster brick
    # convert projection to PROJ4 standard
    if (stringr::str_to_upper(projection) == "LATLONG")
      projection <- "longlat"
    datum <- "NAD83"
    projection_string <- paste("+proj=", projection,
                               " +datum=", datum, 
                               " +ellipsoid=", ellipsoid, 
                               sep = "")
    # define raster coordinates
    xmn <- xOrigin
    ymn <- yOrigin
    xmx <- xOrigin + xDelta * xCount
    ymx <- yOrigin + yDelta * yCount
    basin <- raster::brick(output_vals,  
                       xmn = xmn,
                       xmx = xmx,
                       ymn = ymn,
                       ymx = ymx,
                       crs = projection_string)
    
    # set layer names
    names(basin) <- attribute_names
    
    if (!values_only) {
      # assemble meta data
      meta_data <- data.frame(attribute_names, attribute_type_names, 
                              attribute_type_vals,
                              stringsAsFactors = FALSE)
      return_vals <- list(basin = basin, meta_data = meta_data, 
                          header_lines = header_lines)
      return(return_vals)
    } else {
      # only return basin brick
      return(basin)
    }
  } else {
    # return values as an array
    if (!values_only) {
      # assemble meta data
      meta_data <- data.frame(attribute_names, attribute_type_names, 
                              attribute_type_vals,
                              stringsAsFactors = FALSE)
      return_vals <- list(basin = output_vals, meta_data = meta_data, 
                          header_lines = header_lines)
      return(return_vals)
    } else {
      # only return basin brick
      return(output_vals)
    } 

  }

}