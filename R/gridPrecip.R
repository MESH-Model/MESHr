#' Grids station precipitation
#'
#' @description Grids station interval precipitation values, so that they can
#' be used as MESH inputs. The gridding method is performed by the \pkg{hydroTSM}
#' function \code{hydrokrige}, using the IDW (inverse distance weighting) algorithm.
#'
#' @param precip Required. A list containing 3 elements: 1. the header meta data,
#' 2. the column meta data, and 3. the precipitation values (in mm). These values
#' are returned automatically by the \pkg{MESHr} command \code{read_tb0}. Note
#' that the precipitation values are in mm.
#' @param shed_raster Required. A \code{RasterBrick} object describing the MESH basin.
#' This can be created using the \pkg{MESHr} command \code{read_r2c_shed} with the
#' parameter \code{as_rasters = TRUE}.
#' @param IDW_file Required. Name of the output file which holds gridded precipitation for all
#' time steps.
#' @param quiet Optional. If \code{TRUE} (the default), messages are suppressed.
#' If \code{FALSE}, the time interval and messages from each gridding are listed.
#' @param progress_bar Optional. If \code{TRUE} (the default), a progress bar is
#' displayed showing the completed fraction of the \code{precip}.
#' 
#' @return If unsuccessful, returns \code{FALSE}. If successful, returns
#' \code{TRUE} and the gridded precipitation values are written to the
#' \code{IDW_file}. Note that each interval's precipitation is written as it
#' is gridded. This saves on memory, and will save at least some of the values
#' in case there is a crash, but is slower. The gridded precipitation
#' is in mm/s.
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
#' IDW_file <- "RedDeerPrecip.idw"
#' gridPrecip(precip, shed_raster, IDW_file)
#' }
gridPrecip <- function(precip = NULL, shed_raster = NULL,
                       IDW_file = NULL,
                       quiet = TRUE, progress_bar = TRUE) {
  if (is.null(precip) | is.null(shed_raster) | is.null(IDW_file)) {
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

  # get precip - station names begin with a zero, add a leading 'X'

  zeroes <- grepl("^0", names(precip$data))
  names(precip$data)[zeroes] <- paste("X", names(precip$data)[zeroes], sep = "")

  # assemble lats and longs from ts0 header
  x <- precip$column_meta_data$columnLocationX
  y <- precip$column_meta_data$columnLocationY
  name <- precip$column_meta_data$columnNames
  zeroes <- grepl("^0", name)
  name[zeroes] <- paste("X", name[zeroes], sep = "")

  locs <- data.frame(x, y)
  site_info <- data.frame(locs, name)

  # grid each hour's precip and write to output file
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
  sourcefile <- ":SourceFile   RedDeer_all_hourly.tb0"
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
  for (i in 1:num_rows) {
    h <- as.vector(hourly[i, -1])
    h2 <- as.numeric(h)
    names(h2) <- names(h)
    # if all value are NA, set them to zero
    if (sum(is.na(h2)) > 0) {
      # all values are NA
      h2[is.na(h2)] <- 0
    }

    datetime <- format(hourly[i, 1], format = "%Y/%m/%d %H:%M:%S.0")
    if (!quiet) {
      cat(datetime, "\n")
    }

    frame <- paste(":Frame      ", i, "        ", i, '"', datetime, '"')
    cat(frame, eol, file = IDW_file, append = TRUE)

    p <- hydroTSM::hydrokrige(h2,
      x.gis = site_info,
      X = "x", Y = "y",
      sname = "name", predictors = elev_sp, p4s = p4s,
      plot = FALSE,
      verbose = !quiet, debug.level = 0
    )


    m <- as.matrix(p["var1.pred"])
    rm(p)
    t_m <- t(m)
    t_m[!is.na(t_m)] <- t_m[!is.na(t_m)] / 3600 # mm -> mm/s
    t_m[is.na(t_m)] <- -1
    t_flipped <- apply(t_m, 2, rev)

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
