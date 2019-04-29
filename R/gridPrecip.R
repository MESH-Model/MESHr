#' Grids station precipitation
#'
#' @description Grids station interval precipitation values, so that they can
#' be used as MESH inputs. The station values are distributed over the domain of
#' the basin. Values outside of the basin are set to the missing value.
#' The gridding method is performed by the \pkg{hydroTSM}
#' function \code{hydrokrige}, using the IDW (inverse distance weighting) algorithm
#' for each time interval. If there are no values present in a given interval, then the 
#' entire domain will be set to the missing value. If all of the values are identical,
#' or if there is a single value, then all of the domain will have 
#' that value. 
#' It is possible that the IDW algorithm will be unable to grid the specified values
#' in a given interval. In this case, the error is trapped, and the mean of the station
#' values is used for the entire domain.
#'
#' @param precip Required. A list containing 3 elements: 1. the header meta data,
#' 2. the column meta data, and 3. the precipitation values (in mm). These values
#' are returned automatically by the \pkg{MESHr} command \code{read_tb0}. Note
#' that the precipitation values are in \strong{mm}.
#' @param source_file_name Required. The name of the original \code{.tb0} source file. 
#' Default value is \code{unknown}. The name of the source file is written to
#' the \code{r2c} file header.
#' @param shed_raster Required. A \code{RasterBrick} object describing the MESH basin.
#' This can be read from a MESH r2c shed file using the \pkg{MESHr} command 
#' \code{read_r2c_shed} with the parameter \code{as_rasters = TRUE}.
#' @param IDW_file Required. Name of the output file which will hold the gridded 
#' precipitation for all time intervals.
#' @param missing_value Required. Value to be used if all values in an interval 
#' are missing. Default is \code{0}. Also used to code individual missing values.
#' @param zeromissing Required. If there are enough precipitation values for gridding
#' to take place, but there are still missing values, setting \code{zeromissing = TRUE}
#' will set the missing values to zero, before the gridding takes place. The default value
#' is \code{FALSE}.
#' @param quiet Optional. If \code{TRUE} (the default), messages are suppressed.
#' If \code{FALSE}, the time interval and messages from each gridding are listed.
#' @param progress_bar Optional. This function can take a long time to execute,
#' depending on the number of time intervals, and the number of stations. 
#' If \code{progress_bar = TRUE} (the default), a progress bar is
#' displayed showing the completed fraction of the \code{precip}. If you are calling
#' this function from a Notebook which is to be knitted to a file, then
#' you will probably want to set \code{progress_bar = FALSE}, to avoid
#' having multiple copies of the progress bar in the output file.
#' 
#' @return If unsuccessful, returns \code{FALSE}. If successful, returns
#' \code{TRUE} and the gridded precipitation values are written to the
#' \code{IDW_file}. Note that each interval's precipitation is written as it
#' is gridded. This saves on memory, and will save at least some of the values
#' in case there is a crash, but is slow. The gridded precipitation
#' is in \code{mm/s}.
#' @author Kevin Shook
#' @seealso \code{\link{gridTemp}} \code{\link{read_r2c_shed}} \code{\link{read_tb0}}
#' @export
#'
#' @examples
#' \dontrun{
#' hourly_precip_file <- "Red_Deer_all_hourly_precip_new.tb0"
#' precip <- read_tb0(hourly_precip_file, values_only = FALSE, timezone = "Etc/GMT+7", NAvalue = -0.1)
#' shedfile <- "RedDeer_MESH_drainage_database.r2c"
#' shed_raster <- read_r2c_shed(shedfile, as_rasters = TRUE, values_only = TRUE)
#' IDW_file <- "RedDeerPrecip_idw.r2c"
#' source_file_name <- "Red_Deer_all_hourly_precip_new.tb0"
#' gridPrecip(precip, source_file_name, shed_raster, IDW_file, missing_value = -999)
#' }
gridPrecip <- function(precip = NULL, 
                       source_file_name = "unknown", 
                       shed_raster = NULL,
                       IDW_file = NULL, 
                       missing_value = 0,
                       zeromissing = FALSE,
                       quiet = TRUE, 
                       progress_bar = TRUE) {
  
  # check parameters
  if (is.null(precip) | is.null(shed_raster) | is.null(IDW_file) |
      is.null(source_file_name)) {
    cat("Error: missing values\n")
    return(FALSE)
  }

  # set projection
  prj <- "+proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0"

  # get shed as a raster brick
  elev_raster <- shed_raster$Elev
  elev_raster[elev_raster < 1] <- NA_real_ # set outside basin to NA

  # convert raster to SpatialGridDataFrame
  elev_sp <- methods::as(elev_raster, "SpatialGridDataFrame")

  # get precip - for station names that begin with a zero, add a leading 'X'

  zeroes <- grepl("^0", names(precip$data))
  names(precip$data)[zeroes] <- paste("X", names(precip$data)[zeroes], sep = "")

  # assemble lats and longs from tb0 header
  x <- precip$column_meta_data$columnLocationX
  y <- precip$column_meta_data$columnLocationY
  name <- precip$column_meta_data$columnNames
  zeroes <- grepl("^0", name)
  name[zeroes] <- paste("X", name[zeroes], sep = "")

  locs <- data.frame(x, y)
  # site_info <- data.frame(x, y, site_elev, name)
  # 
  # names(site_info)[3] <- "Elev"
  # grid each hour's precip and write to output file
  site_info <- data.frame(x, y, name)
  hourly <- precip$data
  num_rows <- nrow(hourly)
  rows <- nrow(elev_raster)
  cols <- ncol(elev_raster)

  eol <- "\n"

  # write header
  header1 <- "########################################"
  filetype <- ":FileType r2c  ASCII  EnSim 1.0"
  header1 <- c(header1, filetype, "#")
  datatype <- "# DataType               2D Rect Cell"
  header1 <- c(header1, datatype, "#")
  application <- ":Application    FORTRAN  "
  header1 <- c(header1, application)
  version <- ":Version    1.0.0"
  header1 <- c(header1, version)
  written_by <- ":WrittenBy    MESHr"
  header1 <- c(header1, written_by)
  creation_date <- paste(":CreationDate ", date(), sep = "")
  header1 <- c(
    header1, creation_date, "#",
    "#---------------------------------------"
  )

  name <- ":Name                    PR0"
  header1 <- c(header1, "#", name, "#")
  projection <- ":Projection              LATLONG   "
  header1 <- c(header1, projection)
  ellipsoid <- ":Ellipsoid               WGS84 "
  header1 <- c(header1, ellipsoid, "#")
  e <- raster::extent(elev_raster)
  x_origin <- paste(":xOrigin                 ", e@xmin, sep = "")
  y_origin <- paste(":yOrigin                 ", e@ymin, sep = "")
  header1 <- c(header1, x_origin, y_origin, "#")
  
  # add source file name to header
  sourcefile <- paste(":SourceFile              ",  source_file_name, sep = "")
  header1 <- c(header1, sourcefile, "#")

  attributename <- ":AttributeName           Precipitation_rate"
  attributeunit <- ":AttributeUnit           mm/s"
  header1 <- c(header1, attributename, attributeunit, "#")
  xcount <- paste(":xCount                   ", cols, sep = "")
  ycount <- paste(":yCount                   ", rows, sep = "")
  xdelta <- paste(":xDelta                   ", raster::res(elev_raster)[1], 
                  sep = "")
  ydelta <- paste(":yDelta                   ", raster::res(elev_raster)[2], 
                  sep = "")
  header1 <- c(header1, xcount, ycount, xdelta, ydelta)
  header1 <- c(header1, "#", "#", ":endHeader")


  con <- file(IDW_file, open = "w")
  writeLines(header1, con = con, sep = eol)
  close(con)


  # now loop through hourly precipitation, writing to file
  p4s <- sp::CRS(prj)
  empty <- as.matrix(elev_raster)
  empty[] <- missing_value
  
  all_zero <- as.matrix(elev_raster)
  all_zero[!is.na(all_zero)] <- 0
  
  for (i in 1:num_rows) {
    h <- as.vector(hourly[i, -1])
    h2 <- as.numeric(h)
    names(h2) <- names(h)
    
    goodvals <- !is.na(h2)
    numgood <- length(h2[goodvals])
    
    zerovals <- (h2 <= 0)
    zerovals[is.na(zerovals)] <- FALSE
    numzeros <- length(h2[zerovals])

    datetime <- format(hourly[i, 1], format = "%Y/%m/%d %H:%M:%S.0")
    if (!quiet) {
      cat(datetime, "\n")
    }

    frame <- paste(":Frame       ", i, "         ", i, ' "', datetime, '"', sep = "")
    cat(frame, eol, file = IDW_file, append = TRUE)
    

    
    # if all values are NA, set the entire grid to missing value
    if (numgood == 0) {
      # all values are NA
      h2 <- empty
      t_flipped <- apply(h2, 2, rev)
    } else if (numgood > 0 & numzeros == numgood) {
      # all values are zero
      h2 <- all_zero
      t_flipped <- apply(h2, 2, rev)
    } else if (numgood == 1 & numzeros != numgood ) {
      # all values are the same
      all_same <- as.matrix(elev_raster)
      all_same[!is.na(all_same)] <- (max(h2[goodvals]) / 3600) # mm -> mm/s
      h2 <- all_same
      t_flipped <- apply(h2, 2, rev)
    } else {

      if(zeromissing){
        if (sum(is.na(h2)) > 0) {
          # all values are NA
          h2[is.na(h2)] <- 0
        }
      }
      # more than 1 precip value is present, so interpolate with
      # error trapping
      p <- try(hydroTSM::hydrokrige(h2,
        x.gis = site_info,
        X = "x", Y = "y", 
        sname = "name", predictors = elev_sp, p4s = p4s,
        plot = FALSE,
        verbose = !quiet, debug.level = 0), TRUE
      )
      
      if (class(p) != "try-error") {
        # it worked
        m <- as.matrix(p["var1.pred"])
        rm(p)
        t_m <- t(m)
        t_m[!is.na(t_m)] <- t_m[!is.na(t_m)] / 3600 # mm -> mm/s
        t_m[is.na(t_m)] <- missing_value
        t_flipped <- apply(t_m, 2, rev)
      } else {
        # problem with gridding, so use mean value
        mean_val <- mean(h2, na.rm = TRUE)
        all_same <- as.matrix(elev_raster)
        all_same[!is.na(all_same)] <- (mean_val / 3600) # mm -> mm/s
        h2 <- all_same
        t_flipped <- apply(h2, 2, rev)
      }
    }

    # set NA values to missing value
    t_flipped[is.na(t_flipped)] <- missing_value
    
    for (row in 1:rows) {
      output_str <- formatC(t_flipped[row, ],
        digits = 7, width = 10,
        format = "f")
      cat(output_str, "\n", sep = "  ", file = IDW_file, append = TRUE)
    }

    cat(":EndFrame", eol, file = IDW_file, append = TRUE)

    if (progress_bar) {
      utils::txtProgressBar(
        min = 0, max = num_rows, initial = i, width = 30,
        style = 3)
    }
  }
  return(TRUE)
}
