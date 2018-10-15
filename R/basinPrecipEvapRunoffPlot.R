#' Plots basin precipitation evaporation and runoff
#'
#' @param basinWaterBalance Required. Data frame to be plotted. As read in by \code{readOutputTimeseriesCSV}.
#' @return Returns a \pkg{ggplot2} line plot of the variable values (mm).
#' @author Kevin Shook
#' @seealso \code{\link{readOutputTimeseriesCSV}} \code{\link{basinStoragePlot}} \code{\link{basinSoilWaterIcePlot}}
#' @export
#'
#' @examples \dontrun{
#' waterBalance <- readOutputTimeseriesCSV("Basin_average_water_balance.csv")
#' p <- basinPrecipEvapRunoffPlot(waterBalance)}
basinPrecipEvapRunoffPlot <- function(basinWaterBalance) {
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
  
  varNames <- c("PRE", "EVAP", "ROF")
  
  
  # get selected variables
  non_datetime <- basinWaterBalance[, -1]
  df_var_names <- names(non_datetime)
  
  
  selected_vars <- df_var_names %in% varNames
  selected_df <- non_datetime[, selected_vars]
  
  if (is.null(ncol(selected_df))) {
    # only a single variable is selected
    selected_df <- data.frame(selected_df)
    names(selected_df) <- names(non_datetime)[selected_vars]
  }
  

  g_title <- "Precipitation, Evaporation and Runoff"
  
  # put all columns together
  allvars <- cbind(basinWaterBalance[, 1], selected_df)
  timeVarName <- names(basinWaterBalance)[1]
  names(allvars)[1] <- timeVarName
  
  if (timeVarName == "DATE") {
    melted <- reshape2::melt(allvars, id.vars = "DATE")
    melted$variable <- as.character(melted$variable)
    
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