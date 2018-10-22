#' Plots basin runoff components
#'
#' @param basinWaterBalance Required. Data frame to be plotted. As read in by 
#' \code{read_MESH_OutputTimeseries_csv}.
#' @param cumul Optional. If \code{FALSE}, then interval values are plotted. 
#' If \code{TRUE} (the default) then cumulative values are plotted. Note that the cumulative 
#' values are determined by summing the interval values \emph{not} by plotting the 
#' MESH cumulative variables.
#'
#' @return Returns a \pkg{ggplot2} line plot of the variable values (mm).
#' @author Kevin Shook
#' @seealso \code{\link{read_MESH_OutputTimeseries_csv}} \code{\link{basinStoragePlot}} \code{\link{basinSoilWaterIcePlot}}
#' @export
#'
#' @examples \dontrun{
#' waterBalance <- read_MESH_OutputTimeseries_csv("Basin_average_water_balance.csv")
#' p <- basinRunoffPlot(waterBalance)}
basinRunoffPlot <- function(basinWaterBalance, cumul = FALSE) {
  
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
  
  varNames <- c("ROF", "ROFO", "ROFS", "ROFB")
  
  
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
  
  # make vars cumulative, if requested
  if (cumul) {
    selected_df <- cumsum(selected_df)
    g_title <- "Runoff components (cumulative)"
  } else {
    g_title <- "Runoff components"
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