#' Plots basin precipitation cumulative water balance.
#'@description As with the \code{basinRunoffPlot}, the cumulative values of
#' precipitation, evaporation and runoff (and optionally, delta storage) 
#' are computed by the function, rather than by using the MESH variables. This
#' allows the plot to be used on a sub-set of the basin output data.
#' @param basinWaterBalance Required. Data frame to be plotted. As read in by 
#' \code{readOutputTimeseriesCSV}. Note that because the value of \code{DTSG} 
#' (delta storage) can be negative, you need to set a threshold value much smaller than
#' zero when you read in the values. 
#' @param accumulate_delta_storage Optional. If \code{TRUE} (the default),  
#' the delta storage is accumulated from the beginning of the data set. If
#' \code{FALSE}, the delta storage values in the file are \emph{not} accumulated
#' as they are assumed to be cumualtive values.
#' @return Returns a \pkg{ggplot2} line plot of the variable values (mm).
#' @author Kevin Shook
#' @seealso \code{\link{readOutputTimeseriesCSV}} \code{\link{basinStoragePlot}} \code{\link{basinSoilWaterIcePlot}}
#' @export
#'
#' @examples \dontrun{
#' waterBalance <- readOutputTimeseriesCSV("Basin_average_water_balance.csv", 
#' missingValueThreshold = -1e6)
#' p <- basinWaterBalancePlot(waterBalance)}

basinWaterBalancePlot <- function(basinWaterBalance, accumulate_delta_storage = TRUE) {
  p <- NULL
  DATE <- NULL
  value <- NULL
  variable <- NULL
  datetime <- NULL
  
  # check for data
  if (nrow(basinWaterBalance) < 1) {
    cat("Error: missing values\n")
    return(FALSE)
  }
  
  varNames <- c("PRE", "EVAP", "ROF", "DSTG")
  
  # get selected variables
  non_datetime <- basinWaterBalance[, -1]
  df_var_names <- names(non_datetime)
  
  
  selected_vars <- df_var_names %in% varNames
  selected_df <- non_datetime[, selected_vars]
  
  # do cumulative values
  if (!accumulate_delta_storage) {
    selected_df[,c(1:3)] <- cumsum(selected_df[,c(1:3)])
  } else {
    selected_df[,c(1:4)] <- cumsum(selected_df[,c(1:4)])    
  }

  
  # add net
  selected_df$net <- selected_df$PRE - selected_df$EVAP - 
    selected_df$ROF - selected_df$DSTG
  
  
  if (is.null(ncol(selected_df))) {
    # only a single variable is selected
    selected_df <- data.frame(selected_df)
    names(selected_df) <- names(non_datetime)[selected_vars]
  }
  
  
  g_title <- "Overall water balance - Cumulative"
  
  # put all columns together
  allvars <- cbind(basinWaterBalance[, 1], selected_df)
  timeVarName <- names(basinWaterBalance)[1]
  names(allvars)[1] <- timeVarName
  
  if (timeVarName == "DATE") {
    melted <- reshape2::melt(allvars, id.vars = "DATE")
    melted$variable <- as.character(melted$variable)
    
    # replace variable names
    melted$variable[melted$variable == "DSTG"] = "\u2206 Storage"
    melted$variable[melted$variable == "net"] = "P - E - R - \u2206 S"
    
    # set plotting order
    melted$variable <- factor(melted$variable, levels = c("PRE", "EVAP", 
                                                          "ROF", "\u2206 Storage", 
                                                          "P - E - R - \u2206 S"))
    
    p <- ggplot2::ggplot(melted,
                         ggplot2::aes(DATE, value, colour = variable)) + 
      ggplot2::geom_line() + 
      ggplot2::xlab("") + 
      ggplot2::ylab("Water (mm)")  +
      ggplot2::ggtitle(g_title)
    
  } else {
    melted <- reshape2::melt(allvars, id.vars = "datetime")
    melted$variable <- as.character(melted$variable)
    
    p <- ggplot2::ggplot(melted, ggplot2::aes(datetime, value, colour = variable)) + 
      ggplot2::geom_line() + 
      ggplot2::xlab("") + 
      ggplot2::ylab("Water (mm)")  +
      ggplot2::ggtitle(g_title)
  }
}