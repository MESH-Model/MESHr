#' Calculates Goodness of Fit statistics for MESH output
#' 
#' @description This function is a wrapper for the function \code{gof} in the package \pkg{hydroGOF}. 
#' It computes several Goodness of Fit statistics for each station. The output of this function may be
#' used on its own, or to annotate a hydrograph.
#'
#' @param MESHvals Required. A data frame of output from a MESH run, as produced by \code{readOutputTimeseriesCSV}.
#' @param stationNames Optional. A vector of strings holding station names. If specified, the station names will 
#' be used in the returned data frame, otherwise the MESH station numbers will be used.
#'
#' @return Returns a data frame with the following variables:
#' #' \describe{
#'   \item{station}{station name or number}
#'   \item{me}{Mean Error}
#'   \item{mae}{Mean Absolute Error}
#'   \item{mse}{Mean Squared Error}
#'   \item{rmse}{Root Mean Square Error}
#'   \item{nrmse}{Normalized Root Mean Square Error  ( -100\% <= nrms <= 100\% )}
#'   \item{PBIAS}{Percent Bias }
#'   \item{pbiasfdc}{PBIAS in the slope of the midsegment of the Flow Duration Curve}
#'   \item{RSR}{Ratio of RMSE to the Standard Deviation of the Observations, RSR = rms / sd(obs). ( 0 <= RSR <= +Inf )}
#'   \item{rSD}{Ratio of Standard Deviations, rSD = sd(sim) / sd(obs)}
#'   \item{NSE}{Nash-Sutcliffe Efficiency ( -Inf <= NSE <= 1 )}
#'   \item{mNSE}{Modified Nash-Sutcliffe Efficiency}
#'   \item{rNSE}{Relative Nash-Sutcliffe Efficiency }
#'   \item{d}{Index of Agreement ( 0 <= d <= 1 )}
#'   \item{d1}{Modified Index of Agreement}
#'   \item{rd}{Relative Index of Agreement}
#'   \item{cp}{Persistence Index ( 0 <= PI <= 1 ) }
#'   \item{r}{Pearson Correlation coefficient ( -1 <= r <= 1 )}
#'   \item{r.Spearman}{Spearman Correlation coefficient ( -1 <= r.Spearman <= 1 ) }
#'   \item{R2}{Coefficient of Determination ( 0 <= R2 <= 1 ). \cr
#'     Gives the proportion of the variance of one variable that is predictable from the other variable}
#'   \item{bR2}{R2 multiplied by the coefficient of the regression line between \code{sim} and \code{obs} \cr ( 0 <= bR2 #'   <= 1 )}
#'   \item{KGE}{Kling-Gupta efficiency between \code{sim} and \code{obs} \cr ( 0 <= KGE <= 1 )}
#'   \item{VE}{Volumetric efficiency between \code{sim} and \code{obs} \cr ( -Inf <= VE <= 1)}
#' }
#' @export
#'
#' @examples 
#' stats <- hydroStats(MESH_streamflows)
#' stats$NSE
hydroStats <- function(MESHvals, stationNames = "") {
  vars <- names(MESHvals)
  if (vars[1] != "DATE" & vars[1] != "DATETIME") {
    cat('Error: not a time series date frame\n')
    return(FALSE)
  }
  
  # check for specified station names
  numCols <- ncol(MESHvals) - 1
  numStations <- floor(numCols / 2)
  
  if (length(stationNames) >= numStations) {
    tempstation <- ""
  } else {
    tempstation <- 0
  }
  
  for (i in 1:numStations) {
    meas_col <- (i - 1) * 2 + 2
    sim_col <- meas_col + 1
    meas <- MESHvals[, meas_col]
    sim <- MESHvals[, sim_col]
    g <- hydroGOF::gof(sim, meas)
    g <- as.data.frame(t(g))
    g$station <- tempstation

    if (i == 1) {
      if (length(stationNames) >= numStations) {
        g$station <- stationNames[i]
      } else {
        g$station <- i
      }
      
      gvals <- g
    } else {
      if (length(stationNames) >= numStations) {
        g$station <- stationNames[i]
      } else {
        g$station <- i
      }
      
      gvals <- rbind(gvals, g)
    }
  }
  
  # move station to column 1
  cols <- ncol(gvals)
  gvals <- gvals[, c(cols, 1:(cols - 1))]
  
  return(gvals)
}