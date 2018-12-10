#' Calculates basin-wide lapse rates
#' 
#' @description This function calculates basin-wide historical lapse rates, by
#' month and hour of day. The intent is to produce a file of rates that can
#' be used for interpolation, when there are insufficient values to determine
#' lapse rates from measured air temperatures. The lapse rates are determined
#' as the slope of a linear regression of delta air temperature vs delta elevation
#' for each time step. The delta air temperature is the difference in air 
#' temperature between each station's value and that of the lowest elevation 
#' station. The delta elevation is the difference between each station's elevation
#' and that of the lowest-elevation station. Therefore the lapse rate is in
#' K/m.
#'
#' @param temps Required. A  time series data frame of air temperatures in 
#' Celsius or K, as returned by \code{read_tb0}. The first column must be 
#' \code{datetime}, which is a POSIXct value. Each station's elevation will 
#' be in a separate column.
#' @param elevs Required. A data frame of station elevations. Note that 
#' the first column must contain the station names (which must be the same as
#' in the air temperatures), and the second column must contain the elevation (in m).
#'
#' @return If successful, returns a data frame with 24 rows (one for each hour), and
#' 12 columns (one for each month), containing the lapse rates.
#' @author Kevin Shook
#' @seealso \code{\link{gridTemp}} \code{\link{read_tb0}}
#' @export
#'
#' @examples \dontrun{
#' lapse_rates <- basinLapseRates(temperatures, elevations)}

basinLapseRates <- function(temps = NULL, elevs = NULL) {
 
  if (is.null(temps)) {
    cat("Error: missing air temperatures\n")
    return(FALSE)
  }
  
  if (is.null(elevs)) {
    cat("Error: missing elevations\n")
    return(FALSE)
  }
  
  # assign elevations to temps, and get delta elevation from lowest station.
  temps_names <- names(temps)[-1]
  elev_locs <- match(temps_names, elevs$Station)
  elev_sorted <- elevs[elev_locs, 2]
  site <- elevs[elev_locs, 1]
  min_el <- min(elev_sorted)
  min_loc <- which.min(elev_sorted)

  elev_delta <- elev_sorted - min_el
  elev_delta_df <- data.frame(site, elev_delta)

  # get delta temperature for each station from lowest station temperature
  # for each interval.

  delta_temp <- temps
  delta_temp[,-1] <- temps[ , -1] - temps[,(min_loc + 1)]  # get delta T vs lowest station 

  # melt data set
  melted <- reshape2::melt(delta_temp, id.vars = "datetime")
  names(melted)[c(2, 3)] <- c("site", "delta_t")
  melted$hour_name <- format(melted$datetime, format = "%H")
  melted$month_name <- format(melted$datetime, format = "%b")
  melted$hour_num <- as.numeric(format(melted$datetime, format = "%H"))
  melted$month_num <- as.numeric(format(melted$datetime, format = "%m"))
 
  # do linear model for month x hour and save regression slopes
  months <- seq(1, 12)
  hours <- seq(0, 23)
  lapse <- matrix(data = NA_real_, nrow = length(hours), ncol = length(months))
  merged2 <- merge(melted, elev_delta_df, by = "site")
  cleaned <- na.omit(merged2)
  cleaned$month_name <- factor(cleaned$month_name, levels = month.abb)
  for (monthNum in months) {
    for (hourNum in hours) {
      selected <- cleaned[cleaned$month_num == monthNum & cleaned$hour_num == hourNum ,]
      # do lm
      r <- stats::lm(delta_t ~ elev_delta - 1, selected)
      slope <- as.numeric(r[[1]])
      lapse[hourNum + 1, monthNum] <- slope
    }
  }
  
  # create data frame from matrix
  lapse <- data.frame(lapse, row.names = hours)
  names(lapse) <- month.abb
  
  return(lapse)
}