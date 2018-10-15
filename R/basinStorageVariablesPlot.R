#' Plots basin water balance storage components
#' @description Creates a \pkg{ggplot2} stacked area plot of specified water 
#' balance storage components. The variables plotted may include:
#' \describe{
#'   \item{variable}{definition}
#'   \item{SNCAN}{Snow component of precipitation intercepted by the canopy}
#'   \item{RCAN}{Rain component of precipitation intercepted by the canopy	mm or kg m-2 of water	AVG}
#'   \item{SNO}{Snow water equivalent (SWE) of the snow mass}
#'   \item{ZPND}{Depth of water ponded at the surface}
#'   \item{LQWS}{Water equivalent of the volumetric liquid water content of the soil}
#'   \item{FZWS}{Water equivalent of the volumetric frozen water content of the soil}
#'   \item{ALWS}{Water equivalent of the volumetric liquid and frozen water contents of the soil, sum of LQWS and FZWS}
#'   }
#' @param basinWaterBalance Required. Data frame to be plotted. As read in by \code{readOutputTimeseriesCSV}.
#' @param varNames Optional. A vector of the names of the variables to be plotted. 
#' If not specified, all of the variables listed above will be plotted.
#' @param layers Optional. A vector of the layers to be plotted. By default 
#' layers 1 through 6 are used.
#'
#' @return Returns a \pkg{ggplot2} stacked area time plot of the variable values (mm).
#' @author Kevin Shook
#' @seealso \code{\link{readOutputTimeseriesCSV}}
#' @export
#'
#' @examples \dontrun{
#' waterBalance <- readOutputTimeseriesCSV("Basin_average_water_balance.csv")
#' p <- basinStorageVariablesPlot(waterBalance)
#' # the plot can have a restricted date range
#' startDate <- as.Date("2005-10-01", format = "%Y-%m-%d")
#' endDate <- as.Date("2006-09-30", format = "%Y-%m-%d")
#' library(ggplot2)
#' p <- p + xlim(startDate, endDate)
#' p
#' # you can also change the colours used, either by
#' using a defined scale, or by manually specifying them
#' # This example uses the colours in the package viridis
#' # which scales from dark to light by reversing the direction
#' library(viridis)
#' p <- p +  scale_fill_viridis(discrete = TRUE, direction = -1)
#' p
#' }
basinStorageVariablesPlot <- function(basinWaterBalance, varNames = "", 
                             layers = c(1, 2, 3, 4, 5, 6)) {
  
  # declare ggplot variables
  p <- NULL 
  DATE <- NULL
  value <- NULL
  variable <- NULL
  datetime <- NULL
  
  # check for data
  if (nrow(basinWaterBalance) < 1) {
    cat('Error: missing values\n')
    return(FALSE)
  }
  
  if (varNames == "") {
    varNames <- c("SNCAN", "RCAN", "SNO", "ZPND", "LQWS", "FZWS", "ALWS")
  }
  
  
  # get selected variables
  non_datetime <- basinWaterBalance[, -1]
  df_var_names <- names(non_datetime)
  
  # strip trailing numbers
  df_var_names_nonums <- gsub('[0-9]+', '', df_var_names)
  
  selected_vars <- df_var_names_nonums %in% varNames
  selected_df <- non_datetime[,selected_vars]
  selected_var_names <- names(selected_df)
  selected_var_names_nonums <- gsub('[0-9]+', '', selected_var_names)
  
  # find layer numbers
  last2chars <- stringr::str_sub(selected_var_names_nonums, start = -2) 
  layer_cols <- stringr::str_detect(last2chars, "WS")
  non_layer_cols <- !layer_cols

  layer_col_names <- selected_var_names[layer_cols]
  layer_nums <- stringr::str_split_fixed(layer_col_names, "WS", n = 2)
  layer_nums <- layer_nums[,2]
  layer_nums <- as.numeric(layer_nums)
  selected_layer_nums <- layer_nums %in% layers
  
  non_layer_vars <- selected_df[, non_layer_cols]
  layer_vars <- selected_df[, layer_cols]
  layer_vars <- layer_vars[, selected_layer_nums]
  
  # put all columns together
  allvars <- cbind(basinWaterBalance[,1], non_layer_vars,
                   layer_vars)

  timeVarName <- names(basinWaterBalance)[1]
  names(allvars)[1] <- timeVarName
  
  if (timeVarName == "DATE") {
   melted <- reshape2::melt(allvars, id.vars = "DATE")
   melted$variable <- as.character(melted$variable)
    p <- ggplot2::ggplot(melted, ggplot2::aes(DATE, value, fill = variable)) +
     ggplot2::geom_area( position = "stack") +
     ggplot2::xlab("") +
     ggplot2::ylab("Water (mm)") +
     ggplot2::ggtitle("Storage Variables")
  } else {
    melted <- reshape2::melt(allvars, id.vars = "datetime")
    melted$variable <- as.character(melted$variable)
 
    p <- ggplot2::ggplot(melted, ggplot2::aes(datetime, value, fill = variable)) +
      ggplot2::geom_area(position = "stack")
      ggplot2::xlab("") +
      ggplot2::ylab("Water (mm)") +
      ggplot2::ggtitle("Storage Variables")
  }

  return(p)
}