#' Plots basin soil water and ice
#'
#' @param basinWaterBalance Required. Data frame to be plotted. As read in by \code{read_MESH_OutputTimeseries_csv}.
#' @param layers Optional. A vector of the layers to be plotted. By default layers 1 and 2 are used.
#'
#' @return Returns a \pkg{ggplot2} line plot of the variable values (mm) faceted
#' by Layer number. The name of the variable is \code{layer}, so you can change
#' change the facetting.
#' @author Kevin Shook
#' @seealso \code{\link{read_MESH_OutputTimeseries_csv}} \code{\link{basinStoragePlot}}
#' @export
#'
#' @examples \dontrun{
#' waterBalance <- read_MESH_OutputTimeseries_csv("Basin_average_water_balance.csv")
#' p <- basinSoilWaterIcePlot(waterBalance)}
basinSoilWaterIcePlot <- function(basinWaterBalance, layers = c(1, 2))  {
  
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
  
  varNames <- c("LQWS", "FRWS", "ALWS")
  
  
  # get selected variables
  non_datetime <- basinWaterBalance[, -1]
  df_var_names <- names(non_datetime)
  
  # strip trailing numbers
  df_var_names_nonums <- gsub("[0-9]+", "", df_var_names)
  
  selected_vars <- df_var_names_nonums %in% varNames
  selected_df <- non_datetime[, selected_vars]
  selected_var_names <- names(selected_df)
  selected_var_names_nonums <- gsub("[0-9]+", "", selected_var_names)
  
  # find layer numbers
  last2chars <- stringr::str_sub(selected_var_names_nonums, 
    start = -2)
  layer_cols <- stringr::str_detect(last2chars, "WS")

  layer_col_names <- selected_var_names[layer_cols]
  layer_nums <- stringr::str_split_fixed(layer_col_names, "WS", 
    n = 2)
  layer_nums <- layer_nums[, 2]
  layer_nums <- as.numeric(layer_nums)
  selected_layer_nums <- layer_nums %in% layers
  
  layer_vars <- selected_df[, layer_cols]
  layer_vars <- layer_vars[, selected_layer_nums]
  
  # put all columns together
  allvars <- cbind(basinWaterBalance[, 1], layer_vars)
  
  timeVarName <- names(basinWaterBalance)[1]
  names(allvars)[1] <- timeVarName
  
  if (timeVarName == "DATE") {
    melted <- reshape2::melt(allvars, id.vars = "DATE")
    melted$variable <- as.character(melted$variable)
    
    # separate variable and layer
    layer_nums <- stringr::str_split_fixed(melted$variable, 
      "WS", n = 2)
    layer_nums <- layer_nums[, 2]
    layer_nums <- as.numeric(layer_nums)
    melted$layer <- paste("Layer", layer_nums)
    melted$variable <- stringr::str_sub(melted$variable, 
      end = 4)
    
    p <- ggplot2::ggplot(melted, ggplot2::aes(DATE, value, 
      colour = variable)) + 
      ggplot2::geom_line() + 
      ggplot2::xlab("") + 
      ggplot2::ylab("Water (mm)") + 
      ggplot2::facet_wrap(~layer) +
      ggplot2::ggtitle("Soil Water and Ice")
    
  } else {
    melted <- reshape2::melt(allvars, id.vars = "datetime")
    melted$variable <- as.character(melted$variable)

    p <- ggplot2::ggplot(melted, ggplot2::aes(datetime, value, 
      colour = variable)) + 
      ggplot2::geom_line() + 
      ggplot2::xlab("") + 
      ggplot2::ylab("Water (mm)") + 
      ggplot2::facet_wrap(~layer) +
      ggplot2::ggtitle("Soil Water and Ice")
  }
  
  return(p)
}