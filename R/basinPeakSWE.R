#' Find annual maximum SWE
#'
#' @param waterBalance Requried. Data frame of water balance variables as returned by
#'  \code{read_MESH_OutputTimeseries_csv}.
#'
#' @return Returns a data frame of \code{year}, \code{date_max_SWE}, and \code{max_SWE}. 
#' Note that the SWE is the sum of the variables \code{SNO} and \code{WSNO}.
#' @export
#'
#' @examples \dontrun{waterBalance <- read_MESH_OutputTimeseries_csv("Basin_average_water_balance.csv", 
#' missingValueThreshold = -1e6)
#' basinPeakSWE(waterBalance)}
#' 
basinPeakSWE <- function(waterBalance) {
  # define plyr variables
  SWE <- NULL
  DATE <- NULL
  DATETIME <- NULL
  year <- NULL
  
  # check for data
  if (nrow(waterBalance) < 1) {
    cat("Error: missing values\n")
    return(FALSE)
  }
  
  if (names(waterBalance)[1] == "DATE") {
    SWE <- waterBalance[, c("DATE", "SNO", "WSNO")]
    SWE$SWE <- SWE$SNO + SWE$WSNO
    SWE$year <- as.numeric(format(SWE$DATE, format = "%Y"))
    # get max SWE and date
    
    annual_max <-  plyr::ddply(SWE, .(year), plyr::summarize, 
                   date_max_SWE = DATE[which.max(SWE)],
                   max_SWE      = max(SWE))
    
    return(annual_max)
    
  } else {
    SWE <- waterBalance[, c("DATETIME", "SNO", "WSNO")]
    SWE <- waterBalance[, c("DATE", "SNO", "WSNO")]
    SWE$SWE <- SWE$SNO + SWE$WSNO
    SWE$year <- as.numeric(format(SWE$DATETIME, format = "%Y"))
    # get max SWE and date
    
    annual_max <-  plyr::ddply(SWE, .(year), plyr::summarize, 
                         date_max_SWE = DATETIME[which.max(SWE)],
                         max_SWE      = max(SWE))
    
    return(annual_max)
  }


  

  
}