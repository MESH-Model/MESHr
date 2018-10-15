#' Creates hydrograph from MESH output
#'
#' @description Creates a \pkg{ggplot} hydrograph from MESH output. This function \emph{only} uses 
#' values from a single MESH data frame (as read in using \code{readOutputTimeseriesCSV}), so does
#' not work with outside sources, such as WSC files. Because this function returns a 
#' \pkg{ggplot} object, you can change its format in any way you like.
#' The plots produced may be faceted using the commands \code{facet_wrap} or \code{facet_grid}.
#' @param MESHvals Required. A data frame of output from a MESH run, as produced by \code{readOutputTimeseriesCSV}.
#' @param stationNames Optional. A vector of strings holding station names. If specified, the station names will 
#' be used in the plots. Otherwise the MESH station numbers will be used.
#' @param byStation Optional. If \code{TRUE} (the default) then the plots will be coloured according to the station names. You may want to set this to \code{FALSE} if you are facetting by station name.
#' @param byYear Optional. If \code{TRUE} then the plots will be able to be facetted by year. Note that 
#' this means that the dates are all plotted using the year 2000, so you will see strange results
#'  if you set this to \code{TRUE} and don't facet by year. Default is \code{FALSE}
#' @param meas Optional. Should the measured values be plotted? Default is \code{TRUE}. If \code{FALSE}, they will be omitted.
#' @param sim Optional. Should the simulated values be plotted? Default is \code{TRUE}. If \code{FALSE}, they will be omitted.
#' @param calStart Optional. The start date of the calibration period. Must be a string in the format \option{yyyy-mm-dd}. If specified, values on and after this date will be designated as the \code{Calibration} period. The remaining values will be designated as the \code{Validation} period.
#' @param calEnd Optional. The start date of the calibration period. Must be a string in the format \option{yyyy-mm-dd}. If specified, values on and after this date will be designated as the \code{Calibration} period. The remaining values will be designated as the \code{Validation} period.
#' @return If successful, returns a \pkg{ggplot2} object. If unsuccessful, returns \code{FALSE}. The object can be facetted by the name of the station (the variable is called \code{station}). If the option \code{byYear = TRUE}, then the object can be facetted by the variable \code{YEAR}.
#' @author Kevin Shook
#' @note Specifying the calibration start and/or end dates will allow the resulting plot to be facetted by the variable \code{period}.
#' @seealso \code{\link{readOutputTimeseriesCSV}} \code{\link{hydroStats}}
#' @export
#'
#' @examples 
#' # plot hydrograph of all data on single graph
#' p1 <- simpleHydrograph(MESH_streamflows)
#' p1
#' # add station names, and replot
#' stations <- c("Station1", "Station2")
#' p2 <- simpleHydrograph(MESH_streamflows, stationNames = stations)
#' p2
#' # remove colouring by station, and facet, changing the axis label format
#' p3 <- simpleHydrograph(MESH_streamflows, stationNames = stations, byStation = FALSE)
#' # load in all of ggplot2 to modify plots
#' library(ggplot2)
#' p3 <- p3 + facet_wrap(~station, nrow = 2) + scale_x_date(date_labels = "%Y")
#' p3
#' # plot by year, then facet
#' p4 <- simpleHydrograph(MESH_streamflows, stationNames = stations, byYear = TRUE)
#' p4 <- p4 + facet_wrap(~YEAR, scales = "free_y")
#' p4
#' # remove colouring for stations, and facet by station and year
#' p5 <- simpleHydrograph(MESH_streamflows, stationNames = stations, byStation = FALSE, byYear = TRUE)
#' p5 <- p5 + facet_grid(YEAR~station, scales = "free_y")
#' # change colours
#' plotcols <- c("red", "blue")
#' p5 <- p5 + scale_colour_manual(values = plotcols)
#' p5
simpleHydrograph <- function(MESHvals, stationNames = "", byStation = TRUE, 
                             byYear = FALSE, meas = TRUE, sim = TRUE, calStart = "",
                             calEnd = "") {
  # assign ggplot variables
  DATE <- NULL
  value <- NULL
  type <- NULL
  station <- NULL
  DATETIME <- NULL
  
  # check for time series
  vars <- names(MESHvals)
  if (vars[1] != "DATE" & vars[1] != "DATETIME") {
    cat('Error: not a time series date frame\n')
    return(FALSE)
  }
  
  if (!meas & !sim) {
    cat('Error: nothing to plot\n')
    return(FALSE)
  }
  
  # check for specified station names
  numCols <- ncol(MESHvals) - 1
  numStations <- floor(numCols / 2)
  
  
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

  # melt datasets
  if (vars[1] == "DATE") {
    melted <- reshape2::melt(MESHvals, id.vars = "DATE")
    meas_loc <- stringr::str_detect(melted$var, "MEAS")
    meas_pieces <- stringr::str_split_fixed(melted$var[meas_loc], "QOMEAS", 2)
    meas_vals <- meas_pieces[,2]
    melted$type <- ""
    melted$type[meas_loc] <- "Measured"
    melted$station <- ""
    melted$station[meas_loc] <- as.numeric(meas_vals)
    
    sim_loc <- stringr::str_detect(melted$var, "SIM")
    melted$type[sim_loc] <- "Simulated"
    sim_pieces <- stringr::str_split_fixed(melted$var[sim_loc], "QOSIM", 2)
    sim_vals <- sim_pieces[,2]
    melted$station[sim_loc] <- as.numeric(sim_vals)
    
    # add station name
    if (length(stationNames) >= numStations) {
      melted$station <- stationNames[as.numeric(melted$station)]
    }
    
    # add period
    if ((calStart != "") | (calEnd != "")) {
      melted$period <- "Validation"
      melted$period[(melted$DATE >= calStartDate) & 
                      (melted$DATE <= calEndDate)] <- "Calibration"
    }
    
    # if by year, add year and fakedate
    if (byYear) {
      melted$YEAR <- as.numeric(format(melted$DATE, format = "%Y"))
      melted$DATE <- as.Date(format(melted$DATE, format = "%d-%m-2000"), format = "%d-%m-%Y")
    }
    
    # filter out measured or simulated if not selected
    if (!meas) {
      melted <- melted[melted$type != "Measured", ]
    }
    
    if (!sim) {
      melted <- melted[melted$type != "Simulated", ]
    }
    

    
  # do plot
    if (!byYear & byStation) {
      # all data on a single plot, coloured by station
      # not intended for facetting

      p <- ggplot2::ggplot(melted, ggplot2::aes(DATE, value, colour = station, 
                                                linetype = type)) +
        ggplot2::geom_line() +
        ggplot2::xlab("") +
        ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                       sep = ""))) 
      return(p)
    
    } else if (!byYear & !byStation) {
      p <- ggplot(melted, ggplot2::aes(DATE, value, colour = type)) +
        ggplot2::geom_line() +
        ggplot2::xlab("") +
        ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                       sep = ""))) +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::scale_x_date(date_labels = "%b")
      return(p)
    } else if (!byStation & byYear) {
        p <- ggplot(melted, ggplot2::aes(DATE, value, colour = type)) +
          ggplot2::geom_line() +
          ggplot2::xlab("") +
          ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                         sep = ""))) +
          ggplot2::theme(legend.title = ggplot2::element_blank()) +
          ggplot2::scale_x_date(date_labels = "%b")
        return(p)
    } else if (byStation & byYear) {
        p <- ggplot(melted, ggplot2::aes(DATE, value, colour = station, 
                                         linetype = type)) +
          ggplot2::geom_line() +
          ggplot2::xlab("") +
          ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                         sep = ""))) +
          ggplot2::theme(legend.title = ggplot2::element_blank()) +
          ggplot2::scale_x_date(date_labels = "%b")
      }
    } else {
      # plot by datetime
      melted <- reshape2::melt(MESHvals, id.vars = "DATETIME")
      meas_loc <- stringr::str_detect(melted$var, "MEAS")
      meas_pieces <- stringr::str_split_fixed(melted$var[meas_loc], "QOMEAS", 2)
      meas_vals <- meas_pieces[,2]
      melted$type <- ""
      melted$type[meas_loc] <- "Measured"
      melted$station <- ""
      melted$station[meas_loc] <- as.numeric(meas_vals)
      
      sim_loc <- stringr::str_detect(melted$var, "SIM")
      melted$type[sim_loc] <- "Simulated"
      sim_pieces <- stringr::str_split_fixed(melted$var[sim_loc], "QOSIM", 2)
      sim_vals <- sim_pieces[,2]
      melted$station[sim_loc] <- as.numeric(sim_vals)
      
      # add station name
      if (length(stationNames) >= numStations) {
        melted$station <- stationNames[melted$station]
      }
      
      # add period
      if ((calStart != "") | (calEnd != "")) {
        melted$period <- "Validation"
        melted$period[(as.Date(melted$DATETIME) >= calStartDate) & 
                        (as.Date(melted$DATE) <= calEndDate)] <- "Calibration"
      }
      
      
      # if by year, add year and fakedate
      if (byYear) {
        melted$YEAR <- as.numeric(format(melted$DATETIME, format = "%Y"))
        melted$DATETIME <- as.Date(format(melted$DATE, 
                                          format = "%d-%m-2000 %H:%M"), 
                                   format = "%d-%m-%Y %H:%M")
      }
      
      # filter out measured or simulated if not selected
      if (!meas) {
        melted <- melted[melted$type != "Measured", ]
      }
      
      if (!sim) {
        melted <- melted[melted$type != "Simulated", ]
      }
      

      
      # do plot
      if (!byYear & byStation) {
        p <- ggplot2::ggplot(melted, ggplot2::aes(DATETIME, value, colour = station, 
                                                  linetype = type)) +
          ggplot2::geom_line() +
          ggplot2::xlab("") +
          ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                         sep = ""))) 
        return(p)
        
      } else if (!byYear & !byStation) {
        p <- ggplot(melted, ggplot2::aes(DATETIME, value, colour = type)) +
          ggplot2::geom_line() +
          ggplot2::xlab("") +
          ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                         sep = ""))) +
          ggplot2::theme(legend.title = ggplot2::element_blank()) +
          ggplot2::scale_x_date(date_labels = "%b")
        return(p)
      } else if (!byStation & byYear) {
        p <- ggplot(melted, ggplot2::aes(DATETIME, value, colour = type)) +
          ggplot2::geom_line() +
          ggplot2::xlab("") +
          ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                         sep = ""))) +
          ggplot2::theme(legend.title = ggplot2::element_blank()) +
          ggplot2::scale_x_date(date_labels = "%b")
        return(p)
      } else if (byStation & byYear) {
        p <- ggplot(melted, ggplot2::aes(DATETIME, value, colour = station, 
                                         linetype = type)) +
          ggplot2::geom_line() +
          ggplot2::xlab("") +
          ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                         sep = ""))) +
          ggplot2::theme(legend.title = ggplot2::element_blank()) +
          ggplot2::scale_x_date(date_labels = "%b")
      }
  }
}