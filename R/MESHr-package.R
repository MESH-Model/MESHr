#' @title Functions for MESH pre- and post- processing
#' @docType package
#' @name MESHr-package
#' 
#' @description The intent of this package is to contain functions do do common tasks
#' for MESH modelling, such as reading output, plotting, and assessing model quality. The 
#' first functions will use .csv files - support for netCDF will be added.

#' @references 
#' To cite \pkg{MESHr} in publications, use the command \code{citation("MESHr")} to get the current version of the citation.\cr
#' @import grid ggplot2 reshape2 stringr knitr raster rts
#' @importFrom stats na.omit 
#' @importFrom utils read.table write.table txtProgressBar
#' @importFrom readr read_fwf
#' @importFrom stats lm
#' @importFrom sp CRS
#' @importFrom hydroTSM hydrokrige
#' @importFrom methods as
#' @importFrom hydroGOF gof
NULL
