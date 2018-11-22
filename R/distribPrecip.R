#' Distributes precipitation in time
#'
#' @description MESH requires all forcing data to have the same time interval,
#' which prevents the use of precipitation data reported at lower frequencies
#' than the model time step. This function distributes low-freqency precipitation
#' (e.g. daily) according to a  set of high frequency precipitation 
#' (e.g. hourly). The high-frequency values are summed to have the same time 
#' intervals as the low-frequency data. The ratios of low/high freqency 
#' precipitations are determined for each time step, and these ratios
#' are multiplied by the high-frequency data. Where the high-frequency total
#' precipitation is zero, the low-frequency data is spread evenly over the 
#' high-frequency interval.
#' @param LFprecip Required. A data frame of low temporal frequency (e.g. daily)
#' precipitation. The first column must be a POSIXct date/time called 
#' \code{datetime}. The second column must be the precipitation.
#' @param HFprecip Required. A data frame of high temporal frequency 
#' (e.g. hourly) precipitation. The first column must be a POSIXct date/time 
#' called \code{datetime}. The second column must be the precipitation.
#' @param zero_missing_HF Optional. If \code{TRUE} (the default) 
#' missing high-frequency precipitation values are replaced with zeros.
#' @param period_thresholds Length of maximum infilled period in hours. 
#' Sequences of missing low-frequency values exceeding this length will be set
#' to \code{NA_real_}, and the next low-frequency value will be assigned to
#' the next high frequency interval. This is usually required when a gauge 
#' is not reported for a seasonal period.
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
distribPrecip <- function(LFprecip = NULL, HFprecip = NULL, 
                          zero_missing_HF = TRUE,
                          period_threshold = 48) {
  
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
  
  # set missing HF data to zero if requested
  if (zero_missing_HF)
    merged$HF[is.na(merged$HF)] <- 0

  # find end of LF data and terminate data frame
  merged$rownums <- seq(1:nrow(merged))
  max_LF_row <- max(merged$rownums[!is.na(merged$LF)])
  merged <- merged[1:max_LF_row,]
  
  test <- !is.na(merged$LF) > 0
  y <- cumsum(test)
  merged$periods <- c(y[1], y[-length(y)]) + 1
  runs <- rle(merged$periods)
  run_lengths <- runs$lengths
  run_values <- runs$values
  i <- inverse.rle(runs)
  
  # get precip sum in each run
  period_sums <- aggregate(merged$HF, by = list(merged$periods), FUN = "sum")
  names(period_sums) <- c("period", "HF_sum")
  
  # get HF/LF ratios for each period
  LF <- merged$LF[!is.na(merged$LF)]

  period_sums$LF <- LF
  period_sums$ratio <- LF / period_sums$HF_sum
  period_sums$length <- run_lengths
  
  # distribute precip evenly over the interval if there is no HF precip
  period_sums$even_dist <- NA_real_
  infinite_locs <- which(is.infinite(period_sums$ratio))
  period_sums$even_dist[infinite_locs] <- period_sums$LF[infinite_locs] /
    period_sums$length[infinite_locs]
  
  merged$ratio <- period_sums$ratio[merged$periods]
  merged$ratio[is.nan(merged$ratio)] <- 0
  merged$distributedP <- merged$HF * merged$ratio
  merged$period_length <- period_sums$length[merged$periods]
  
  merged$even_dist <- period_sums$even_dist[merged$periods]
  merged$distributedP[is.infinite(merged$ratio)] <- 
    merged$even_dist[is.infinite(merged$ratio)] 
  
  # set all but last merged value to NA, when run length > threshold
  merged$distributedP[merged$period_length > period_threshold] <- NA_real_
  last_p_locs <- (merged$period_length > period_threshold) & !is.na(merged$LF) 
  merged$distributedP[last_p_locs] <- merged$LF[last_p_locs]
  
  # return distributed values
  distributed <- merged[,c("datetime", "distributedP")]
  return(distributed)

}