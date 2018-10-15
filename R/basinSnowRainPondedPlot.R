#' Plots basin snow and rain in the canopy and ponded water
#'
#' @param basinWaterBalance Required. Data frame to be plotted. As read in by \code{readOutputTimeseriesCSV}.
#'
#' @return Returns a \pkg{ggplot2} line plot of the basin variables (mm).
#' @author Kevin Shook
#' @seealso \code{\link{readOutputTimeseriesCSV}} \code{\link{basinStoragePlot}}
#' @export
#'
#' @examples \dontrun{
#' waterBalance <- readOutputTimeseriesCSV("Basin_average_water_balance.csv")
#' p <- basinSnowRainPondedPlot(waterBalance)}

basinSnowRainPondedPlot <- function(basinWaterBalance){
  # declare ggplot variables
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
  
  varNames <- c("SNCAN", "RCAN", "PNDW")
  
  
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
      ggplot2::ggtitle("Snow, Rain on Canopy and Ponded Water")
    
  } else {
    melted <- reshape2::melt(allvars, id.vars = "datetime")
    melted$variable <- as.character(melted$variable)
    
    p <- ggplot2::ggplot(melted, ggplot2::aes(datetime, value, colour = variable)) + 
      ggplot2::geom_line() + 
      ggplot2::xlab("") + 
      ggplot2::ylab("Water (mm)")  +
      ggplot2::ggtitle("Snow, Rain on Canopy and Ponded Water")
  }
  
  return(p)
}