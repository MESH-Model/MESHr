#' Calculates Goodness of Fit statistics for MESH output
#' 
#' @description This function is a wrapper for the function \code{gof} in the package \pkg{hydroGOF}. 
#' It computes several Goodness of Fit statistics for each station. The output of this function may be
#' used on its own, or to annotate a hydrograph.
#'
#' @param MESHvals Required. A data frame of output from a MESH run, as produced by \code{readOutputTimeseriesCSV}.
#' @param stationNames Optional. A vector of strings holding station names. If specified, the station names will 
#' be used in the returned data frame, otherwise the MESH station numbers will be used.
#' @param calStart Optional. The start date of the calibration period. Must be a string in the format \option{yyyy-mm-dd}. If specified, values on and after this date will be designated as the \code{Calibration} period. The remaining values will be designated as the \code{Validation} period.
#' @param calEnd Optional. The start date of the calibration period. Must be a string in the format \option{yyyy-mm-dd}. If specified, values on and after this date will be designated as the \code{Calibration} period. The remaining values will be designated as the \code{Validation} period.
#' @param removeMissing Required. Should rows with missing values be removed before statistics are calculted? Default is \code{TRUE}
#' @param digits Optional. The number of decimal places for rounding goodness of fit statistics. If \code{0} the default, then it is not set. Default value is \code{2}. Note that percentages like NRMSE and PBIAS will only use a maximum of one decimal place.
#' @param doSpearman Optional. Should Spearman correlation be computed? Default is \code{FALSE}.
#' @param doPBFDC Optional. Should percent bias of slope of the midsegment of the FDC be computed? Default is \code{FALSE}. 
#' @param doLogNSE Optional. Should the NSE of the log-transformed flows be calculated? 
#' Default is \code{TRUE}.
#' @param epsilon Optional. Value to be added to flows when calculating LogNSE, to prevent
#' calculating the log of zero. The default is \option{Pushpalatha2012}, which is
#' 1 percent of the mean observed values. A number may aslo be specified.
#' @param j Optional. Argument passed to the \code{mNSE} function in \pkg{hydroGOF}.
#' @param norm Optional. Argument passed to the \code{nrmse} function in \pkg{hydroGOF}.
#' @param s Optional. Argument passed to the \code{KGE} function in \pkg{hydroGOF}.
#' @param method Optional. Argument passed to the \code{KGE} function in \pkg{hydroGOF}.
#' @param lQ.thr Optional. Argument passed to the \code{pbiassfdc} function in \pkg{hydroGOF}.
#' @param hQ.thr Optional. Argument passed to the \code{pbiassfdc} function in \pkg{hydroGOF}.
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
#'   \item{pbiasfdc}{PBIAS in the slope of the midsegment of the Flow Duration Curve, if selected}
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
#'   \item{r.Spearman}{Spearman Correlation coefficient ( -1 <= r.Spearman <= 1 ), if selected}
#'   \item{R2}{Coefficient of Determination ( 0 <= R2 <= 1 ). \cr
#'     Gives the proportion of the variance of one variable that is predictable from the other variable}
#'   \item{bR2}{R2 multiplied by the coefficient of the regression line between \code{sim} and \code{obs} \cr ( 0 <= bR2 #'   <= 1 )}
#'   \item{KGE}{Kling-Gupta efficiency between \code{sim} and \code{obs} \cr ( 0 <= KGE <= 1 )}
#'   \item{VE}{Volumetric efficiency between \code{sim} and \code{obs} \cr ( -Inf <= VE <= 1)}
#'   \item{LogNSE}{NSE of log-transformed flows, if selected}
#' }
#' If the calibration period is specified, then statistics will be computed separately for the Calibration and Validation periods. The period names and dates will be specified in additional columns.
#' @author Kevin Shook
#' @seealso \code{\link{simpleHydrograph}} \code{\link{gof}}
#' @export
#'
#' @examples 
#' stats <- hydroStats(MESH_streamflows)
#' stats$NSE
#' periodStats <- hydroStats(MESH_streamflows, calEnd = "2010-01-01")
#' periodStats[,1:7]
hydroStats <- function(MESHvals, stationNames = "", calStart = "",
                       calEnd = "", removeMissing = TRUE, 
                       doSpearman = FALSE,
                       doPBFDC = FALSE,
                       doLogNSE = TRUE,
                       epsilon = "Pushpalatha2012",
                       digits = 2,
                       j = 1, 
                       norm = "sd", s = c(1, 1, 1), 
                       method = c("2009", "2012"), 
                       lQ.thr = 0.7, hQ.thr = 0.2) {
  
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
  
  # check for periods
  if (calEnd != "") {
    calEndDate <- as.Date(calEnd)
  } else {
    calEndDate <- as.Date(max(MESHvals[,1]))
  }
  
  if (calStart != "") {
    calStartDate <- as.Date(calStart)
  } else {
    calStartDate <- as.Date(min(MESHvals[,1]))
  }
  
  # add period
  period <- FALSE
  if ((calStart != "") | (calEnd != "")) {
    period <- TRUE
    MESHvals$period <- "Validation"
    MESHvals$period[(as.Date(MESHvals[,1]) >= calStartDate) & 
                    (as.Date(MESHvals[,1]) <= calEndDate)] <- "Calibration"
  }
  
  if (!period) {  
    for (i in 1:numStations) {
      meas_col <- (i - 1) * 2 + 2
      sim_col <- meas_col + 1
      meas <- MESHvals[, meas_col]
      sim <- MESHvals[, sim_col]
      
      # check meas and sim to make sure that they can be used
      if (all(is.na(meas)) | all(is.na(sim)) |
          all(meas <= 0, na.rm = TRUE) | all(meas <= 0, na.rm = TRUE)) {
        cat("Error: No values to use for calculations\n")
        return(FALSE)
      } else {
        
        g <- hydroGOF::gof(sim, meas, na.rm = removeMissing,
                           do.spearman = doSpearman,
                           do.pbfsc = doPBFDC,
                           j = j,
                           norm = norm,
                           s = s,
                           method = method,
                           lQ.thr = lQ.thr,
                           hQ.thr = hQ.thr,
                           digits = digits)
        
        g <- as.data.frame(t(g))
        
        # do log NSE if required
        if (doLogNSE) {
          if (is.numeric(epsilon)) {
            if (epsilon > 0) {
              epsilon_value <- epsilon
              l_nse <- hydroGOF::NSE(sim, meas, na.rm = removeMissing,
                                     FUN = log10, epsilon = "other",
                                     epsilon.value = epsilon_value)
            } else {
              l_nse <- hydroGOF::NSE(sim, meas, na.rm = removeMissing,
                                     FUN = log10, epsilon = "0")
            }
          } else {
            l_nse <- hydroGOF::NSE(sim, meas, na.rm = removeMissing,
                                   FUN = log10, epsilon = epsilon)
          }
          g$LogNSE <- l_nse 
        }

        g$station <- tempstation
        
        if (i == 1) {
          if (length(stationNames) >= numStations & all(stationNames != "")) {
            g$station <- stationNames[i]
          } else {
            g$station <- i
          }
          
          gvals <- g
        } else {
          if (length(stationNames) >= numStations & all(stationNames != "")) {
            g$station <- stationNames[i]
          } else {
            g$station <- i
          }
          
          gvals <- rbind(gvals, g)
        }
      }  # for loop
    } 
    # move station to column 1
    cols <- ncol(gvals)
    gvals <- gvals[, c(cols, 1:(cols - 1))]
    } else {
      # subdivide by period
      validation <- MESHvals[MESHvals$period == "Validation", ]
      calibration <- MESHvals[MESHvals$period == "Calibration", ]
      
      # do calibration first
      for (i in 1:numStations) {
        meas_col <- (i - 1) * 2 + 2
        sim_col <- meas_col + 1
        meas <- calibration[, meas_col]
        sim <- calibration[, sim_col]
        
        
        g <- hydroGOF::gof(sim, meas, na.rm = removeMissing,
                           do.spearman = doSpearman,
                           do.pbfsc = doPBFDC,
                           j = j,
                           norm = norm,
                           s = s,
                           method = method,
                           lQ.thr = lQ.thr,
                           hQ.thr = hQ.thr,
                           digits = digits)
        
        g <- as.data.frame(t(g))
        
        if (doLogNSE) {
          if (is.numeric(epsilon)) {
            if (epsilon > 0) {
              epsilon_value <- epsilon
              l_nse <- hydroGOF::NSE(sim, meas, na.rm = removeMissing,
                                     FUN = log10, epsilon = "other",
                                     epsilon.value = epsilon_value)
            } else {
              l_nse <- hydroGOF::NSE(sim, meas, na.rm = removeMissing,
                                     FUN = log10, epsilon = "0")
            }
          } else {
            l_nse <- hydroGOF::NSE(sim, meas, na.rm = removeMissing,
                                   FUN = log10, epsilon = epsilon)
          }
          g$LogNSE <- l_nse 
        }
        
        g$station <- tempstation
        
        if (i == 1) {
          if (length(stationNames) >= numStations & all(stationNames != "")) {
            g$station <- stationNames[i]
          } else {
            g$station <- i
          }
          
          gvals1 <- g
        } else {
          if (length(stationNames) >= numStations & all(stationNames != "")) {
            g$station <- stationNames[i]
          } else {
            g$station <- i
          }
          gvals1 <- rbind(gvals1, g)
        }  
      }
      # do validation
      for (i in 1:numStations) {
        meas_col <- (i - 1) * 2 + 2
        sim_col <- meas_col + 1
        meas <- validation[, meas_col]
        sim <- validation[, sim_col]
        
        
        g <- hydroGOF::gof(sim, meas,
                           do.spearman = doSpearman,
                           do.pbfsc = doPBFDC,
                           j = j,
                           norm = norm,
                           s = s,
                           method = method,
                           lQ.thr = lQ.thr,
                           hQ.thr = hQ.thr,
                           digits = digits)
        g <- as.data.frame(t(g))

        if (doLogNSE) {
          if (is.numeric(epsilon)) {
            if (epsilon > 0) {
              epsilon_value <- epsilon
              l_nse <- hydroGOF::NSE(sim, meas, na.rm = removeMissing,
                                     FUN = log10, epsilon = "other",
                                     epsilon.value = epsilon_value)
            } else {
              l_nse <- hydroGOF::NSE(sim, meas, na.rm = removeMissing,
                                     FUN = log10, epsilon = "0")
            }
          } else {
            l_nse <- hydroGOF::NSE(sim, meas, na.rm = removeMissing,
                                   FUN = log10, epsilon = epsilon)
          }
          g$LogNSE <- l_nse 
        }
        g$station <- tempstation
        
        if (i == 1) {
          if (length(stationNames) >= numStations & all(stationNames != "")) {
            g$station <- stationNames[i]
          } else {
            g$station <- i
          }
          
          gvals2 <- g
        } else {
          if (length(stationNames) >= numStations & all(stationNames != "")) {
            g$station <- stationNames[i]
          } else {
            g$station <- i
          }
          
          gvals2 <- rbind(gvals2, g)
        }  
      }
      # add period names and dates
      gvals1$period <- "Calibration"
      gvals1$start_date <- calStartDate
      gvals1$end_date <- calEndDate
      
      gvals2$period <- "Validation"
      gvals2$start_date <- min(as.Date(validation[,1]))
      gvals2$end_date <- max(as.Date(validation[,1]))
      # combine both data sets
      gvals <- rbind(gvals1, gvals2)  
      # move station to column 1
      cols <- ncol(gvals)
      last_4 <- seq((cols - 3), cols)
      gvals <- gvals[, c(last_4 , 1:(cols - 3))]
  }    

  return(gvals)
}