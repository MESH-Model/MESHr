#' Reads MESH output .csv file containing timeseries
#'
#' @description Reads a file containing any output from a MASH model into a standard \R data frame. 
#' The names of the varables will be trimmed to remove leading and trailing spaces, and the time
#' variables are combined into a single \R date or datetime.
#' @param outputFile Required. Name of MESH output file. Must be a .csv file.
#' @param timezone Not required for daily time series. Required for sub-daily time series. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. You can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time, respectively. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#' @param missingValueThreshold Optional. Any value smaller than this value will be set to \code{NA_real_} when the file is imported. The default value is -0.1 to prevent zero values from being affected.
#'
#' @export 
#' @return If successful, returns a data frame. The first columns will be called 
#' \option{DATE} for daily values, and will contain a standard \R date. For sub-daily timeseries
#' the first column will be called \option{DATETIME} and will contain a standard \code{POSIXct} 
#' date/time. If unsuccessful, returns the value \code{FALSE}.
#' @author Kevin Shook
#' @seealso \code{\link{simpleHydrograph}}
#' @examples \dontrun{
#' timezone <- 'etc/GMT+6'
#' outfile <- "Basin_average_water_balance_ts.csv"
#' output <- read_MESH_OutputTimeseries_csv(outfile, timezone)}
read_MESH_OutputTimeseries_csv <- function(outputFile, timezone = "", missingValueThreshold = -0.1){
  # check parameters
  if (outputFile == '') {
    cat('Error: must specify a file name\n')
    return(FALSE)
  }
  
  output <- read.table(file = outputFile, header = TRUE, 
                       stringsAsFactors = FALSE, sep = ",", strip.white = TRUE)
  
  # remove empty columns caused by trailing commas
  good_columns <- !is.na(output[1, ])
  output <- output[, good_columns]
 
  # check for sub-daily output
  vars <- names(output)
  
  # trim spaces from names
  vars <- trimws(vars, which = "both")
  names(output) <- vars
  
  # make sure it's actually a time series
  if ( vars[1] != "YEAR" | vars[2] != "DAY") {
    cat('Error: not a time series file\n')
    return(FALSE)
  }
  
  if (vars[3] == "HOUR") {
    subdaily <- TRUE
  } else {
    subdaily <- FALSE
  }
  
  if ( subdaily & (timezone == "")) {
    cat('Error: must specify a timezone\n')
    return(FALSE)
  }
  
  if (!subdaily) {
    DATE <- paste(output$YEAR, "-", output$DAY, sep = "")
    DATE <- as.Date(DATE, format = "%Y-%j")
    returned_df <- data.frame(DATE, output[, -(1:2)])
     # set missing values to NA
    vals <- returned_df[,-1]
    vals[vals < missingValueThreshold] <- NA_real_
    returned_df[,-1] <- vals
    return(returned_df)
  } else {
    # check to see if sub-hourly
    
    if (vars[4] == "MINS") {
      DATETIME <-  paste(output$YEAR, "-", output$DAY, " ",
                         output$HOUR, ":", output$MINS, sep = "")
      
      DATETIME <- as.POSIXct(DATETIME, tz = timezone, format = "%Y-%j %H:%M")
      returned_df <- data.frame(DATETIME, output[, -(1:4)])
      # set missing values to NA
      vals <- returned_df[,-1]
      vals[vals < missingValueThreshold] <- NA_real_
      returned_df[,-1] <- vals
      return(returned_df)
      return(returned_df)
    } else {
      DATETIME <-  paste(output$YEAR, "-", output$DAY, " ",
                         output$HOUR, sep = "")
      
      DATETIME <- as.POSIXct(DATETIME, tz = timezone, format = "%Y-%j %H:%M")
      returned_df <- data.frame(DATETIME, output[, -(1:3)])
      # set missing values to NA
      vals <- returned_df[,-1]
      vals[vals < missingValueThreshold] <- NA_real_
      returned_df[,-1] <- vals
      return(returned_df)
      return(returned_df)
    }
  }
}