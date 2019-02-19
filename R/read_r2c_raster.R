#' Reads r2c file to raster brick
#' @description This function reads a file
#' containing a time series of 2D values, which is output from
#' a MESH model. It is not intended to read in a file
#' describing a drainage basin. For that purpose, you should be using the 
#' function \code{read_r2c_shed}. This function returns eiher a \pkg{raster} \code{brick} or 
#' an \pkg{rts} 
#' \code{rts} object, which is a timeseries raster. Each Frame in the original 
#' file becomes a separate layer. 
#' 'The name of each layer in the \pkg{raster} \code{brick} is set to the 
#' time stamp of each Frame. Because the layer names
#' are standard \R variables, they must obey the rules for variable names,
#' inclusing beginning with a character, and not containing spaces. These rule
#' will change the layer names if you are not careful.
#' @param r2cFile Required. Name of \code{r2c} file containing time series.
#' @param NAvalue Optional. If specified, values smaller than \code{NAvalue}
#' will be set to \code{NA_real_}
#' @param as_rts Optional. If \code{TRUE}, the returned value will
#' be a \pkg{rts} object, which allows the creation of 1-D time series, and for
#' temporal aggregation. If \code{FALSE} (the default) a standard \pkg{raster} 
#' \code{brick} object is returned, which is better for simple plotting of
#' the layers.
#' @param timezone Optional. If the \code{r2cFile} contains date values for 
#' each Frame, then the Frame times are returned as \R dates. If there are 
#' hours and seconds, then they will be converted to \code{POSIXct} datetime
#' values. In this case, you may want to specify the \code{timezone} of the data.
#' If the \code{timezone} is not specified, your default value will be used.
#' @param layerNameFormat Optional. Sets the layer names when returning the
#' \pkg{raster} \code{brick} to avoid conflicting with the \R variable rules.
#'
#' @return Returns eiher a \pkg{raster} \code{brick} or an \pkg{rts} 
#' \code{rts} object.
#' @author Kevin Shook
#' @seealso \code{\link{rts}} \code{\link{read_r2c_shed}}
#' @export
#'
#'
#' @examples \dontrun{
#' temps <- read_r2c_raster("TA_M.r2c", NAvalue = 0, as_rts = FALSE, layerNameFormat = "%b_%Y")
#' # convert air temps from K to C
#' temps <- temps - 273.15
#' plot(temps)
#' # create an animation and save it as a file
#' library(animation)
#' saveGIF(animate(temps, n = 1))}
read_r2c_raster <- function(r2cFile, NAvalue = NULL, as_rts = FALSE, timezone = "",
                       layerNameFormat = NULL) {
  
  # read in file
  con <- file(r2cFile, "r", blocking = FALSE, encoding = "ISO_8859-2")
  r2c <- readLines(con)
  close(con)
  
  # parse file
  # first get header info
  
  
  data_type <- findRecord(r2c, "# DataType")
  var_name <- findRecord(r2c, ":Name ")
  projection <- findRecord(r2c, ":Projection")
  
  # try to read ellipsoin
  
  ellipsoid <- try(findRecord(r2c, ":Ellipsoid"))
  
  if(class(ellipsoid) == "try-error")
    ellipsoid <- "WGS84"
  
  xOrigin <- as.numeric(findRecord(r2c, ":xOrigin"))
  yOrigin <- as.numeric(findRecord(r2c, ":yOrigin"))  
  xCount <- as.numeric(findRecord(r2c, ":xCount"))  
  yCount <- as.numeric(findRecord(r2c, ":yCount"))   
  xDelta <- as.numeric(findRecord(r2c, ":xDelta"))  
  yDelta <- as.numeric(findRecord(r2c, ":yDelta")) 
  
  # find number of frames
  frame_start <- grep(":Frame", r2c, fixed = TRUE)
  frame_end <- grep(":EndFrame", r2c, fixed = TRUE)
  # if no frames, terminate as this is not a time series file
  if (length(frame_start) == 0) {
    cat("Error: not a time series file\n")
    return(FALSE)
  }
  
  frame_count <- length(frame_start)
  
  all_frame_vals <- array(data = 0, dim = c(yCount, xCount, frame_count))
  
  # figure out column widths
  widths <- rep.int(15, xCount)
  
  # define raster
  xmn <- xOrigin
  ymn <- yOrigin
  xmx <- xOrigin + xDelta * xCount
  ymx <- yOrigin + yDelta * yCount
  
  # convert projection to PROJ4 standard
  if (stringr::str_to_upper(projection) == "LATLONG")
    projection <- "longlat"
  
  projection_string <- paste("+proj=", projection,
                             " +datum=", ellipsoid, sep = "")
  
  # now read in frames
  datetime_string <- c(0)
  
  # get frames starts and ends
  
  
  for (i in 150:frame_count) {
    frame_header <- r2c[frame_start[i]]
    # extract date/time from header - split by quotes that define it
    datetime_string[i] <- stringr::str_split_fixed(frame_header, 
                                                stringr::fixed('"'), 3)[[2]]
    # trim
    datetime_string[i] <- stringr::str_trim(datetime_string[i], "both")
  
  
    frame_lines <- r2c[(frame_start[i] + 1):(frame_end[i] - 1)]
    frame_lines <- paste(frame_lines, collapse = " ")
    
    # read in as text connection
    con <- textConnection(frame_lines)
    frame_vals <- scan(con, what = "numeric")
    close(con)
    # convert values to a matrix and flip vertically
    frame_vals <- as.numeric(frame_vals)
    frame_matrix <- matrix(data = frame_vals, nrow = yCount,
                           ncol = xCount, byrow = TRUE)
    frame_matrix_flipped <- apply(frame_matrix, 2, rev)
    all_frame_vals[,,i] <- frame_matrix_flipped
  }
  
  # convert matrix to raster brick
  r <- raster::brick(all_frame_vals,  
                       xmn = xmn,
                       xmx = xmx,
                       ymn = ymn,
                       ymx = ymx,
                       crs = projection_string)
  
   # set missing values
  if (!is.null(NAvalue)) {
    r[r <= NAvalue] <- NA_real_
  }
  
  
  if (stringr::str_length(datetime_string[1]) > 10)
    datetimes <- as.POSIXct(datetime_string, format = "%Y/%m/%d %H:%M:%S", tz = timezone)
  else
    datetimes <- as.Date(datetime_string, format = "%Y/%m/%d") 
  
  if (as_rts) {
    # convert to raster time series
    r <- rts::rts(r, time = datetimes)
    } else {
    if (!is.null(layerNameFormat))
      names(r) <- format(datetimes, format = layerNameFormat)
    else
      names(r) <- datetime_string
   }
   
  return(r)
}