#' Reads MESH output .ts file containing timeseries
#'
#' @description Reads a file containing any output from a MESH model as a .ts file into a standard \R data frame. 
#' @param tsFile Required. Name of MESH output file. Must be a .ts file.
#' @param variableNames Optional. The names of the variables as a string vector. 
#' If not specified, the variables will be names \option{Var1}, \option{Var2}, etc.
#' @param timezone Not required for daily time series. Required for sub-daily time series. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. You can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time, respectively. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#' @param missingValueThreshold Optional. Any value smaller than this value will be set to \code{NA_real_} when the file is imported. The default value is -0.1 to prevent zero values from being affected.
#'
#' @export 
#' @return If successful, returns a data frame. The first columns will be called 
#' \option{DATE} for daily values, and will contain a standard \R date. For sub-daily timeseries
#' the first column will be called \option{DATETIME} and will contain a standard \code{POSIXct} 
#' date/time. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{read_MESH_OutputTimeseries_csv}}
#' @examples \dontrun{
#' timezone <- 'etc/GMT+6'
#' outfile <- "SNO_D_GRD.ts"
#' output <- read_MESH_ts(outfile, timezone)}
read_MESH_OutputTimeseries_ts <- function(tsFile, variableNames = "", 
                                          timezone = "", 
                                          missingValueThreshold = -0.1){
  # check parameters
  if (tsFile == '') {
    cat('Error: must specify a file name\n')
    return(FALSE)
  }
  
  output <- read.table(file = tsFile, header = FALSE, 
                       stringsAsFactors = FALSE, strip.white = TRUE)
  
  # convert first column to datetime
  names(output)[1] <- "datetime"
  
  # split 1st col to see if all hours and minutes are the same
  pieces <- stringr::str_split_fixed(output[,1], " ", 2)
  hourmins <- pieces[, 2]
  dates <- pieces[, 1]
  # split again
  pieces <- stringr::str_split_fixed(hourmins, ":", 3)
  hours <- pieces[, 1]
  
  # check if all hours are the same
  if (length(unique(hours)) == 1) {
    # all hours are the same, so daily vals
    dates <- as.Date(dates, format = "%Y/%m/%d")
    output[,1] <- dates
    names(output)[1] <- "DATE"
  } else {
    output[,1] <- as.POSIXct(output[,1], format = "%Y/%m/%d %H:%M:%S",
                             tz = timezone)
    names(output)[1] <- "DATETIME"
  }
  
  returned_df <- output
  
  # set names
  if (variableNames != "") {
    names(returned_df)[-1] <- variableNames
  } else {
    numcols <- ncol(returned_df)
    vars <- numcols - 1
    varnums <- seq(1, vars)
    variableNames <- paste("Var", varnums, sep = "")
    names(returned_df)[-1] <- variableNames
  }
  
  # set missing values to NA
  vals <- returned_df[,-1]
  vals[vals < missingValueThreshold] <- NA_real_
  returned_df[,-1] <- vals
  return(returned_df)
}