#' Reads csv file produced by Alberta Environment and Parks
#'
#' @param AEPfile Required. Name of AEP file to be read in.
#' @param timezone Optional. The name of the timezone of the data as a character string.
#' If the \code{timezone} is not specified, your default value (i.e. your
#' time zone) will be used.  This should be the timezone of your data, but 
#' omitting daylight savings time. Note that the timezone code is specific 
#' to your OS. To avoid problems, you should use a timezone without daylight 
#' savings time. Under Windows or OSX, you can use \option{etc/GMT+6} or 
#' \option{etc/GMT+7} for Central Standard and Mountain Standard time. Under Linux
#' you should use \option{Etc/GMT+6} or \option{Etc/GMT+7}.
#' @param values_only optional. If \code{TRUE} (the default), only the 
#' time series values will be returned. If {FALSE}, the meta data will also be 
#' returned.
#'
#' @return Returns the time series data as a data frame, with the POSIXct 
#' variable \code{datetime} as the time stamp. Note that the time series
#' interval may be irregular. If \code{values_only = TRUE}, then
#' the returned value will be a list conisting of the time series
#' data frame and the header meta data as a list, with the variables 
#' \code{values} and \code{header_meta}, respectively. The meta data are:
#'  \describe{
#'  \item{variable}{type}
#'  \item{station_site}{character}
#'  \item{station_name}{character}
#'  \item{station_number}{character}
#'  \item{parameter_name}{character}
#'  \item{parameter_type}{character}
#'  \item{parameter_type_name}{character}
#'  \item{time_series_name}{character}
#'  \item{time_series_unit}{character}
#'  \item{longitude}{numeric}
#'  \item{latitude}{numeric}
#'  }
#'
#' @export
#' @author Kevin Shook
#' @seealso \code{\link{read_MESH_OutputTimeseries_csv}} 
#' @examples \dontrun{
#' precip <-  read_AEP_csv("05CA805 Skoki Lodge - PC - C.Merged - All.csv", 
#' values_only = FALSE)
#' # show values
#' head(precip$values)
#' # show latitude
#' precip$header_meta$latitude}
read_AEP_csv <- function(AEPfile = "", timezone = "", values_only = TRUE) {
  if (AEPfile == "") {
    cat("Error: missing AEP file\n")
    return(FALSE)
  }
  
  # read in header
  if (!values_only) {
    con <- file(AEPfile)
    header_lines <- readLines(con, n = 15, encoding = "latin1")
    close(con)
    
    # remove not UTF-8
    
    station_site <- findRecord(header_lines, "Station Site:")
    station_name <- findRecord(header_lines, "Station Name:")
    station_number <- findRecord(header_lines, "Station Number:")
    parameter_name <- findRecord(header_lines, "Parameter Name:")
    parameter_type <- findRecord(header_lines, "Parameter Type:")
    parameter_type_name <- findRecord(header_lines, "Parameter Type Name:")
    time_series_name <- findRecord(header_lines, "Time series Name:")
    time_series_unit <- findRecord(header_lines, 
                                   "Time series Unit:")
    longitude <- as.numeric(findRecord(header_lines, "Longitude:"))
    latitude <- as.numeric(findRecord(header_lines, "Latitude:"))
  }

  # read in rest of file
  values <- read.table(AEPfile, skip = 16, sep = ",", header = FALSE,
                       stringsAsFactors = FALSE,  
                       na.strings = "---", 
                       colClasses = c("character", "character", "numeric"))
  names(values) <- c("date", "time", "value")
  
  values$datetime <- paste(values$date, " ", values$time, sep = "")
  if (timezone != "")
    values$datetime <- as.POSIXct(values$datetime, format = "%Y-%m-%d %H:%M:%S",
                                tz = timezone)
  else
    values$datetime <- as.POSIXct(values$datetime, format = "%Y-%m-%d %H:%M:%S")
  
  values <- values[,c("datetime", "value")]
  
  if (nrow(values) == 0) {
    cat("Error: no time series values in AEP file\n")
    return(FALSE)
  }
  
  if (!values_only) {
    # return header meta data and values
    header_meta <- list(
      "station_site" = station_site,
      "station_name" = station_name,
      "station_number" = station_number,
      "parameter_name" = parameter_name,
      "parameter_type" = parameter_type,
      "parameter_type_name" = parameter_type_name,
      "time_series_name" = time_series_name,
      "time_series_unit" = time_series_unit,
      "longitude" = longitude,
      "latitude" = latitude
    )
    output_vals <- list("header_meta" = header_meta, 
                        "values" = values)
    return(output_vals)
  } else {
    return(values)
  }
  
}