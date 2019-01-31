#' Time plots of variable completeness
#'
#' @param MESHdata Required. A data frame of MESH time series, as returned by 
#' \code{read_tb0}
#'
#' @return Returns a \pkg{ggplot2} wrapped facetted plot of data completeness 
#' (as a line) for each station. Missing periods are indicated by gaps in the
#' line. The plot is facetted by the variable \code{station} - so you can change
#' the facetting.
#' @author Kevin Shook
#' @seealso \code{\link{read_tb0}} 
#' @export
#' @examples \dontrun{
#' qvals <- read_tb0("MESH_input_streamflow.tb0", NAvalue = -0.01, values_only = TRUE)
#' p <- plotDataCompleteness(qvals)
#' # change facetting
#' # the y-axis is unimportant, so the plots can be squished vertically
#' library(ggplot2)
#' p <- p + facet_wrap(~station, ncol = 2)}


plotDataCompleteness <- function(MESHdata) {
  # set up ggplot2 variables
  datetime <- NULL
  value <- NULL
  date <- NULL
  
  # test for missing data
  if (is.null(MESHdata)) {
    cat("Error: missing data frame\n")
    return(FALSE)
  }
  
  # set good data = 1
  MESHdata[,-1][!is.na(MESHdata[,-1])] <- 1
  
  # check to see if daily or sub-daily values
  if (names(MESHdata)[1] == "datetime") {
    # melt data
    melted <- reshape2::melt(MESHdata, id.vars = "datetime",
                             variable.name = "station")
    
    p <- ggplot2::ggplot(melted, ggplot2::aes(datetime, value)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~station) +
      ggplot2::xlab("") +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::ylab("Data present")
    
  } else {
    melted <- reshape2::melt(MESHdata, id.vars = "date")
    
    p <- ggplot2::ggplot(melted, ggplot2::aes(date, value)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~station) +
      ggplot2::xlab("") +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::ylab("Data present")
  }

  return(p)
  
}