#' Grids station temperatures
#' 
#'@description Grids station interval temperature values, so that they can 
#'be used as MESH inputs. The gridding method is performed by the \pkg{hydroTSM}
#'function \code{hydrokrige}, using the IDW (inverse distance weighting) algorithm.
#'The gridding uses a basin-scale lapse rate, which is determined by fitting a 
#'linear model to the difference between each site's temperature and that of the
#'lowest site, and the difference in elevation relative to the lowest site. The 
#'procedure is the same as used in the the function \code{basinLapseRates}.
#'In effect, all site temperatures are converted to have the same elevation before 
#'gridding. After gridding, each temperature is raised to its specified elevation
#'using the same lapse rate. Where there are only 1 or 2 stations with available
#'air temperatures, the hourly x monthly lapse rates returned by \code{basinLapseRates}
#'are used.
#' @param temp Required. A list containing 3 elements: 1. the header meta data, 
#' 2. the column meta data, and 3. the air temperature values (in C). These values
#' are returned automatically by the \pkg{MESHr} command \code{read_tb0}.
#' @param source_file_name Required. The name of the original \code{.tb0} source file. 
#' Default value is \code{unknown}. The name of the source file is written to
#' the \code{r2c} file header.
#' @param shed_raster Required. A \code{RasterBrick} object describing the MESH basin. 
#' This can be created using the \pkg{MESHr} command \code{read_r2c_shed} with the 
#' parameter \code{as_rasters = TRUE}.
#' @param site_elev Required. A data frame of station elevations. Note that 
#' the first column must contain the station names (which must be the same as
#' in the air temperatures), and the second column must contain the elevation 
#' (in m).
#' @param lapse_rates Optional. If there are 2 or fewer air temperatures in any
#' interval, then the lapse rate cannot be calculated. In this case, if the
#' historical \code{lapse_rates} are specified, then they will be used. If they are
#' not specified, then this function will terminate with an error message. So,
#' if you are confident that your dataset always has at least 3 stations with non-
#' missing values of air temperatures, then you can omit this parameter. Note that
#' the historical lapse rates must be a data frame of 12 columns (monthly) and
#' 24 rows (hourly) values as returned by the function \code{basinLapseRates}.
#' @param IDW_file Required. Output file which holds gridded air temperatures for all
#' time steps. 
#' @param tmin Required. The minimum permitted air temperature of the gridded 
#' (and lapsed) air temperatures. All values exceeding \code{tmin} will be
#' set to this value. The default is 223.15 K, or -50 C.
#' @param tmax Required. The maximum permitted air temperature of the gridded 
#' (and lapsed) air temperatures. All values exceeding \code{tmax} will be
#' set to this value. The default is 313.15 K, or 40 C.
#' @param missing_value Required.  Value to be used if all values in an interval 
#' are missing. Default is \code{NA_real_}.
#' @param quiet Optional. If \code{TRUE} (the default) messages are suppressed. 
#' If \code{FALSE}, the time interval and messages from each gridding are listed.
#'@param progress_bar Optional. If \code{TRUE} (the default), a progress bar is
#' displayed showing the completed fraction of the \code{temp}.
#' 
#' @return If unsuccessful, returns \code{FALSE}. If successful, returns 
#' \code{TRUE} and the gridded temperatures values are written to the 
#' \code{IDW_file}. Note that each interval's temperatures are written as they
#' is gridded. This saves on memory, but can be quite slow. Note that the 
#' air temperatures in the file are in K.
#' @author Kevin Shook
#' @seealso \code{\link{gridPrecip}} \code{\link{basinLapseRates}} \code{\link{read_r2c_shed}} \code{\link{read_tb0}}
#' @export
#'
#' @examples \dontrun{
#' hourly_temp_file <- "Red_Deer_all_hourly_temp_new.tb0"
#' temp <- read_tb0(hourly_temp_file, values_only = FALSE, timezone = "Etc/GMT+7", NAvalue = -0.1)
#' source_file_name <- hourly_temp_file
#' shedfile <- "RedDeer_MESH_drainage_database.r2c"
#' shed_raster <- read_r2c_shed(shedfile, as_rasters = TRUE, values_only = TRUE)
#' elev_file <- "site_elevations.csv"
#' site_elev <- read.csv(elev_file, header = TRUE, stringsAsFactors = FALSE)
#' lapse_rates_file <- "RedDeerLapseRates.csv"
#' lapse_rates <- read.csv(lapse_rates_file, header = TRUE, stringsAsFactors = FALSE, row.names = 1)
#' IDW_file <- "RedDeerTemp.idw"
#' gridPrecip(temp = temp, source_file_name = source_file_name, 
#' shed_raster = shed_raster, site_elev = site_elev,
#' lapse_rates = lapse_rates)}
gridTemp <- function(temp = NULL,
                     source_file_name = "unknown",
                     shed_raster = NULL, 
                     site_elev = NULL, lapse_rates = NULL, 
                     IDW_file = NULL, tmin = 223.15,
                     tmax = 313.15,
                     missing_value = NA_real_,
                     quiet = TRUE, progress_bar = TRUE) {
  
  if (is.null(temp) | is.null(shed_raster) | is.null(IDW_file) |
      is.null(site_elev) | is.null(tmin) | is.null(tmax)) {
    cat("Error: missing values\n")
    return(FALSE)
  }
  
  # set projection
  prj <- "+proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0"
  
  # get shed as a raster brick
  elev_raster <- shed_raster$Elev
  elev_raster[elev_raster < 1] <- NA_real_  # set outside basin to NA

  # convert raster to SpatialGridDataFrame
  elev_sp <- methods::as(elev_raster, "SpatialGridDataFrame")

  # where station names begin with a zero, add a leading 'X'
  zeroes <- grepl('^0', names(temp$data))
  names(temp$data)[zeroes] <- paste("X", names(temp$data)[zeroes], sep = '')
 
  # assemble lats and longs from ts0 header
  lon <- temp$column_meta_data$columnLocationX
  lat <- temp$column_meta_data$columnLocationY
  name <- temp$column_meta_data$columnNames
  locs <- list(lon, lat)
  names(locs) <- c("lon", "lat")
  
  #  where elevation station names begin with a zero, add a leading 'X'
  zeroes <- grepl('^0', site_elev[,1])
  site_elev[zeroes,1] <- paste("X", site_elev[zeroes, 1], sep = '')
  
  
  # select elevations of sites and find min elevation and
  # delta elevations for all sites
  
  hourly <- temp$data
  temps_names <- names(hourly)[-1]
  elev_locs <- match(temps_names, site_elev[,1])
  elev_sorted <- site_elev[elev_locs, 2]
  min_el <- min(elev_sorted)
  min_loc <- which.min(elev_sorted)

  
  # add leading "X" to meta data column names beginning with zero
  name <- temp$column_meta_data$columnNames
  zeroes <- grepl('^0', name)
  name[zeroes] <- paste("X", name[zeroes], sep = '')
  site_info <- data.frame(locs, elev_sorted, name)
  names(site_info)[3] <- "Elev"
  
  
  # grid each hour's temp and write to output file
  num_rows <- nrow(hourly)
  rows <- nrow(elev_raster)
  cols <- ncol(elev_raster)
  
  eol <- "\n"
  
  # construct header
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
  header1 <- c(header1, creation_date, "#",
               "#---------------------------------------")
  
  name <- ":Name                    PR0"
  header1 <- c(header1, "#", name, "#")
  projection <- ":Projection              LATLONG   "
  header1 <- c(header1, projection)
  ellipsoid <- ":Ellipsoid               WGS84 "
  header1 <- c(header1, ellipsoid, "#")
  e <- extent(elev_raster)
  x_origin <- paste(":xOrigin                 ", e@xmin, sep = "" )
  y_origin <- paste(":yOrigin                 ", e@ymin, sep = "" )
  header1 <- c(header1, x_origin, y_origin, "#")
  # add source file name to header
  sourcefile <- paste(":SourceFile              ", source_file_name, sep = "")
  header1 <- c(header1, sourcefile, "#")
  
  attributename <- ":AttributeName           Air_temp"
  attributeunit <- ":AttributeUnit           K"
  header1 <- c(header1, attributename, attributeunit, "#")
  xcount <- paste(":xCount                   ", cols, sep = "")
  ycount <- paste(":yCount                   ", rows, sep = "")
  xdelta <- paste(":xDelta                   ", res(elev_raster)[1], sep = "")
  ydelta <- paste(":yDelta                   ", res(elev_raster)[2], sep = "")
  header1 <- c(header1, xcount, ycount, xdelta, ydelta)
  header1 <- c(header1, "#", "#", ":endHeader")
  
  # write header to file
  con <- file(IDW_file, open = "w")
  writeLines(header1, con = con, sep = eol)
  close(con)

  # now loop through hourly temps, writing to file
  p4s <- CRS(prj)
  empty <- as.matrix(elev_raster)
  empty[] <- missing_value
  
  for (i in 1:num_rows) {
    datetime <- format(hourly[i, 1], format = "%Y/%m/%d %H:%M:%S.0")
    if (!quiet)
      cat(datetime, "\n")
    
    hournum <- as.numeric(format(hourly[i, 1], format = "%H"))
    monthnum <- as.numeric(format(hourly[i, 1], format = "%m"))
    
    # select non-NA values
    h <- as.vector(hourly[i,-1])
    h2 <- as.numeric(h)
    names(h2) <- names(h)
    goodvals <- !is.na(h2)
    numgood <- length(h2[goodvals])
    
    
    # if all values are NA, set the entire grid to missing value
    if (numgood == 0) {
      # all values are NA
      h2 <- empty
      t_flipped <- apply(h2, 2, rev)
    } else {
      good_el <- site_info[goodvals, 3]
      min_el <- min(good_el)
      min_loc <- which.min(good_el)
      delta_el <- good_el - min_el
      good_t <- h2[goodvals]
      delta_t <- good_t - good_t[min_loc]
      
      # get lapse rate for this interval
      if (numgood <= 2) {
        if (!is.null(lapse_rates)) {
          lapse_rate <- lapse_rates[(hournum + 1), monthnum]
        } else {
          cat("Error: not enough air temps to calculate lapse rate on:", 
              datetime, "\n")
          return(FALSE)
        }
      } else {
        # find lowest available station and use to calculate delta_el and 
        # delta_t
        
        f <- lm(delta_t ~ delta_el-1)
        lapse_rate <- f[[1]]
      }
      
      # using the lapse rate, adjust all of the air temps to have the same
      # elevation as the lowest one
      
      adjusted_temps <- good_t - delta_el * lapse_rate
      
      # more than 1 value is present, so interpolate with
      # error trapping
      p <- try(hydroTSM::hydrokrige(adjusted_temps, x.gis = site_info[goodvals,], 
                      elevation = "Elev", X = "lon", Y = "lat", 
                      type = "cells",
                      sname = "name",  predictors = elev_sp,
                      p4s = p4s, 
                      catchment.name = "all",
                      plot = FALSE, verbose = !quiet, debug.level = 0), TRUE)
      
      if (class(p) != "try-error") {
        # it worked
      
        # re-adjust for lapse rate
        p@data <- p@data + ((elev_sp$Elev - min_el) * lapse_rate)
        
        m <- as.matrix(p["var1.pred"])
        rm(p)
        t_m <- t(m)
        
        t_flipped <- apply(t_m, 2, rev)
        
        # set min and max values
        t_flipped <- pmax(t_flipped, tmin)
        t_flipped <- pmin(t_flipped, tmax)
      } else {
        # interpolation failed
        # get mean air temperature
        mean_temp <- mean(adjusted_temps, na.rm = TRUE)
        all_same <- as.matrix(elev_raster)
        all_same[!is.na(all_same)] <- (mean_temp) 
        
        # adjust using lapse rate
        all_same2 <- mean_temp + ((elev_sp$Elev - min_el) * lapse_rate)
        
        h2 <- matrix(all_same2, nrow = nrow(all_same), byrow = TRUE)
        t_flipped <- apply(h2, 2, rev)
        
        # set min and max values
        t_flipped <- pmax(t_flipped, tmin)
        t_flipped <- pmin(t_flipped, tmax)
        
      }

    }
    
    # set NA values to missing value
    t_flipped[is.na(t_flipped)] <- missing_value
    # output gridded values to file
    
    frame <- paste(":Frame       ", i, "         ", i, ' "', datetime, '"', sep = "")
    cat(frame, eol, file = IDW_file, append = TRUE)
    for (row in 1:rows) {
      output_str <- formatC(t_flipped[row,], digits = 7, width = 10, format =  "f")
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