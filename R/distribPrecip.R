#' Distributes precipitation in time
#'
#' @description MESH requires all forcing data to have the same time interval,
#' which prevents the use of precipitation data reported at lower frequencies
#' than the model time step. This function distributes low-freqency precipitation
#' (e.g. daily) according to a  set of high frequency precipitation 
#' (e.g. hourly). The high-frequency values are summed to have the same time 
#' intervals as the low-frequency data. The ratios of low/high freqency 
#' precipiations are determined for each time step, and these ratios
#' are multiplied by the high-frequency data
#' @param LFprecip Required. A data frame of low temporal frequency (e.g. daily)
#' precipitation. The first column must be a POSIXct date/time called 
#' \code{datetime}. The second column must be the precipitation.
#' @param HFprecip Required. A data frame of high temporal frequency 
#' (e.g. hourly) precipitation. The first column must be a POSIXct date/time 
#' called \code{datetime}. The second column must be the precipitation.
#'
#' @return Returns a data frame of the adjusted high-frequency precipitation, 
#' with the variables \code{datetime} and \code{distributedP}. 
#' Note that the last date/time in the returned data corresponds to the final
#' value in the low-frequency data.
#' @author Kevin Shook
#' @seealso \code{\link{read_tb0}}
#' @export
#'
#' @examples \dontrun{distributed <- distribPrecip(myPrecip[, c(1, 5)], 
#' adjacentP)
#' }
distribPrecip <- function(LFprecip = NULL, HFprecip = NULL) {
  
  if (is.null(LFprecip)) {
    cat("Error: missing Low Frequency file\n")
    return(FALSE)
  }
  
  if (is.null(HFprecip)) {
    cat("Error: missing High Frequency file\n")
    return(FALSE)
  }
  
  # merge low freq into high freq
  merged <- merge(HFprecip, LFprecip, by = "datetime", all.x = TRUE)
  names(merged) <- c("datetime", "HF", "LF")
  
  # find end of LF data and terminate data frame
  merged$rownums <- seq(1:nrow(merged))
  max_LF_row <- max(merged$rownums[!is.na(merged$LF)])
  merged <- merged[1:max_LF_row,]
  
  test <- !is.na(merged$LF) > 0
  y <- cumsum(test)
  merged$periods <- c(y[1], y[-length(y)]) + 1
  
  # get precip sum in each run
  period_sums <- aggregate(merged$HF, by = list(merged$periods), FUN = "sum")
  names(period_sums) <- c("period", "HF_sum")
  
  # get HF/LF ratios for each period
  LF <- merged$LF[!is.na(merged$LF)]


  period_sums$ratio <- LF / period_sums$HF_sum
  period_sums$ratio[is.infinite(period_sums$ratio)] <- 0
  merged$ratio <- period_sums$ratio[merged$periods]
  merged$ratio[is.nan(merged$ratio)] <- 0
  merged$distributedP <- merged$HF * merged$ratio
  
  # return distributed values
  distributed <- merged[,c("datetime", "distributedP")]
  return(distributed)

}