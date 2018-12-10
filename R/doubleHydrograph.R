#' Creates hydrograph from 2 MESH output files
#'
#' @description Creates a \pkg{ggplot} hydrograph from MESH output. This function \emph{only} uses 
#' values from two MESH data frames (as read in using \code{readOutputTimeseriesCSV}). It is assumed
#' that the observed data are the same in both cases - only the simulations differ.
#' Because this function returns a \pkg{ggplot} object, you can change its format in any way you like.
#' The plots produced may be faceted using the commands \code{facet_wrap} or \code{facet_grid}.
#' @param MESHvals1 Required. A data frame of output from a MESH run, as produced by \code{read_MESH_OutputTimeseries_csv}.
#' @param stationNames1 Optional. A vector of strings holding station names. If specified, the station names will 
#' be used in the plots. Otherwise the MESH station numbers will be used.
#' @param MESHname1 Optional. A string giving the name of the first MESH output. Default is "MESH1".
#' @param MESHvals2 Required. A data frame of output from a MESH run, as produced by \code{read_MESH_OutputTimeseries_csv}.
#' @param stationNames2 Optional. A vector of strings holding station names. 
#' @param MESHname2 Optional. A string giving the name of the second MESH output. Default is "MESH2".
#' @param byStation Optional. If \code{TRUE} (the default) then the plots will be coloured according to the station names. You may want to set this to \code{FALSE} if you are facetting by station name.
#' @param byYear Optional. If \code{TRUE} then the plots will be able to be facetted by year. Note that 
#' this means that the dates are all plotted using the year 2000, so you will see strange results
#'  if you set this to \code{TRUE} and don't facet by year. Default is \code{FALSE}
#' @param meas Optional. Should the measured values be plotted? Default is \code{TRUE}. If \code{FALSE}, they will be omitted.
#' @param sim Optional. Should the simulated values be plotted? Default is \code{TRUE}. If \code{FALSE}, they will be omitted.
#' @param calStart Optional. The start date of the calibration period. Must be a string in the format \option{yyyy-mm-dd}. If specified, values on and after this date will be designated as the \code{Calibration} period. The remaining values will be designated as the \code{Validation} period.
#' @param calEnd Optional. The start date of the calibration period. Must be a string in the format \option{yyyy-mm-dd}. If specified, values on and after this date will be designated as the \code{Calibration} period. The remaining values will be designated as the \code{Validation} period.
#' @param alpha Optional. Sets the alpha channel (transparency) of the plots. The default value is \code{1}, i.e. opaque. Setting \code{alpha} to less than 1 makes the plots transparent, which can be useful to see overlapping hydrographs.
#' @return If successful, returns a \pkg{ggplot2} object. If unsuccessful, returns \code{FALSE}. The object can be facetted by the name of the station (the variable is called \code{station}). If the option \code{byYear = TRUE}, then the object can be facetted by the variable \code{YEAR}.
#' @author Kevin Shook
#' @note Specifying the calibration start and/or end dates will allow the resulting plot to be facetted by the variable \code{period}.
#' @seealso \code{\link{simpleHydrograph}} \code{\link{read_MESH_OutputTimeseries_csv}} \code{\link{hydroStats}}
#' @export
#'
#' @examples \dontrun{
#' p <- doubleHydrograph(MESHvals1 = capa, MESHname1 = "CaPa precip", 
#' MESHvals2 = kevin, MESHname2 = "Station precip",  byStation = TRUE, 
#' alpha = 0.6)
#' p
#' # facet by station
#' p2 <- p + facet_wrap(~station, scales = "free_y", strip.position = "right") 
#' p2
#' # add scale colour
#' colours <- c("red", "blue", "black")
#' p3 <- p2 + + scale_colour_manual(values = colours)
#' # restrict plot to a range of dates
#' p3
#' startDate <- as.Date("2005-04-01")
#' endDate <- as.Date("2005-10-01")
#' p4 <- p3 + scale_x_date(limits = c(startDate, endDate))
#' p4
#' }
#' 
doubleHydrograph <- function(MESHvals1, stationNames1 = "", MESHname1 = "MESH1",
                             MESHvals2, stationNames2 = "", MESHname2 = "MESH2",
                             byStation = TRUE, 
                             byYear = FALSE, meas = TRUE, sim = TRUE, calStart = "",
                             calEnd = "", alpha = 1) {
  # assign ggplot variables
  DATE <- NULL
  value <- NULL
  type <- NULL
  station <- NULL
  DATETIME <- NULL
  
  # check for time series
  vars1 <- names(MESHvals1)
  if (vars1[1] != "DATE" & vars1[1] != "DATETIME") {
    cat('Error: not a time series date frame\n')
    return(FALSE)
  }
  
  vars2 <- names(MESHvals2)
  if (vars2[1] != "DATE" & vars2[1] != "DATETIME") {
    cat('Error: not a time series date frame\n')
    return(FALSE)
  }
  
  if (!meas & !sim) {
    cat('Error: nothing to plot\n')
    return(FALSE)
  }
  
  # check for specified station names
  numCols1 <- ncol(MESHvals1) - 1
  numStations1 <- floor(numCols1 / 2)
  
  numCols2 <- ncol(MESHvals2) - 1
  numStations2 <- floor(numCols2 / 2)
  
  
  if (calEnd != "") {
    calEndDate <- as.Date(calEnd)
  } else {
    calEndDate <- as.Date(max(MESHvals1[,1]))
  }
  
  if (calStart != "") {
    calStartDate <- as.Date(calStart)
  } else {
    calStartDate <- as.Date(min(MESHvals1[,1]))
  }

  # melt datasets
  if (vars1[1] == "DATE") {
    # have daily values - assume both outputs are the same
    
    # data set 1
    melted1 <- reshape2::melt(MESHvals1, id.vars = "DATE")
    meas_loc1 <- stringr::str_detect(melted1$var, "MEAS")
    meas_pieces1 <- stringr::str_split_fixed(melted1$var[meas_loc1], "QOMEAS", 2)
    meas_vals1 <- meas_pieces1[,2]
    melted1$type <- ""
    melted1$type[meas_loc1] <- "Measured"
    melted1$station <- ""
    melted1$station[meas_loc1] <- as.numeric(meas_vals1)
    
    sim_loc1 <- stringr::str_detect(melted1$var, "SIM")
    melted1$type[sim_loc1] <- MESHname1
    sim_pieces1 <- stringr::str_split_fixed(melted1$var[sim_loc1], "QOSIM", 2)
    sim_vals1 <- sim_pieces1[,2]
    melted1$station[sim_loc1] <- as.numeric(sim_vals1)
    
    # add station name
    if (length(stationNames1) >= numStations1) {
      melted1$station <- stationNames1[as.numeric(melted1$station)]
    }
    
    # add period
    if ((calStart != "") | (calEnd != "")) {
      melted1$period <- "Validation"
      melted1$period[(melted1$DATE >= calStartDate) & 
                      (melted1$DATE <= calEndDate)] <- "Calibration"
    }
    
    # if by year, add year and fakedate
    if (byYear) {
      melted1$YEAR <- as.numeric(format(melted1$DATE, format = "%Y"))
      melted1$DATE <- as.Date(format(melted1$DATE, format = "%d-%m-2000"), 
                              format = "%d-%m-%Y")
    }
    
    # filter out measured or simulated if not selected
    if (!meas) {
      melted1 <- melted1[melted1$type != "Measured", ]
    }
    
    if (!sim) {
      melted1 <- melted1[melted1$type != MESHname1, ]
    }
    
    # data set 2
    melted2 <- reshape2::melt(MESHvals2, id.vars = "DATE")
    meas_loc2 <- stringr::str_detect(melted2$var, "MEAS")
    meas_pieces2 <- stringr::str_split_fixed(melted2$var[meas_loc2], "QOMEAS", 2)
    meas_vals2 <- meas_pieces2[,2]
    melted2$type <- ""
    melted2$type[meas_loc2] <- "Measured"
    melted2$station <- ""
    melted2$station[meas_loc2] <- as.numeric(meas_vals2)
    
    sim_loc2 <- stringr::str_detect(melted2$var, "SIM")
    melted2$type[sim_loc2] <- MESHname2
    sim_pieces2 <- stringr::str_split_fixed(melted2$var[sim_loc2], "QOSIM", 2)
    sim_vals2 <- sim_pieces2[,2]
    melted2$station[sim_loc2] <- as.numeric(sim_vals2)
    
    # add station name
    if (length(stationNames2) >= numStations2) {
      melted2$station <- stationNames2[as.numeric(melted2$station)]
    }
    
    # add period
    if ((calStart != "") | (calEnd != "")) {
      melted2$period <- "Validation"
      melted2$period[(melted2$DATE >= calStartDate) & 
                       (melted2$DATE <= calEndDate)] <- "Calibration"
    }
    
    # if by year, add year and fakedate
    if (byYear) {
      melted2$YEAR <- as.numeric(format(melted2$DATE, format = "%Y"))
      melted2$DATE <- as.Date(format(melted2$DATE, format = "%d-%m-2000"), 
                              format = "%d-%m-%Y")
    }
    
    # filter out measured or simulated if not selected
    if (!meas) {
      melted2 <- melted2[melted2$type != "Measured", ]
    }
    
    if (!sim) {
      melted2 <- melted2[melted2$type != MESHname2, ]
    }

    # combine melted data sets
    melted <- rbind(melted1, melted2)

  # do plot
    if (!byYear & byStation) {
      # all data on a single plot, coloured by type

      p <- ggplot2::ggplot(melted, ggplot2::aes(DATE, value, colour = type)) +
        ggplot2::geom_line(alpha = alpha) +
        ggplot2::xlab("") +
        ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                       sep = ""))) 
      return(p)
    
    } else if (!byYear & !byStation) {
      p <- ggplot(melted, ggplot2::aes(DATE, value, colour = type)) +
        ggplot2::geom_line(alpha = alpha) +
        ggplot2::xlab("") +
        ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                       sep = ""))) +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::scale_x_date(date_labels = "%b")
      return(p)
    } else if (!byStation & byYear) {
        p <- ggplot(melted, ggplot2::aes(DATE, value, colour = type)) +
          ggplot2::geom_line(alpha = alpha) +
          ggplot2::xlab("") +
          ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                         sep = ""))) +
          ggplot2::theme(legend.title = ggplot2::element_blank()) +
          ggplot2::scale_x_date(date_labels = "%b")
        return(p)
    } else if (byStation & byYear) {
        p <- ggplot(melted, ggplot2::aes(DATE, value, colour = station, 
                                         linetype = type)) +
          ggplot2::geom_line(alpha = alpha) +
          ggplot2::xlab("") +
          ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                         sep = ""))) +
          ggplot2::theme(legend.title = ggplot2::element_blank()) +
          ggplot2::scale_x_date(date_labels = "%b")
      }
    } else {
      # plot by datetime
      
      # data set 1
      melted1 <- reshape2::melt(MESHvals1, id.vars = "DATETIME")
      meas_loc1 <- stringr::str_detect(melted1$var, "MEAS")
      meas_pieces1 <- stringr::str_split_fixed(melted1$var[meas_loc1], "QOMEAS", 2)
      meas_vals1 <- meas_pieces1[,2]
      melted1$type <- ""
      melted1$type[meas_loc1] <- "Measured"
      melted1$station <- ""
      melted1$station[meas_loc1] <- as.numeric(meas_vals1)
      
      sim_loc1 <- stringr::str_detect(melted1$var, "SIM")
      melted1$type[sim_loc1] <- MESHname1
      sim_pieces1 <- stringr::str_split_fixed(melted1$var[sim_loc1], "QOSIM", 2)
      sim_vals1 <- sim_pieces1[,2]
      melted1$station[sim_loc1] <- as.numeric(sim_vals1)
      
      # add station name
      if (length(stationNames1) >= numStations1) {
        melted1$station <- stationNames1[melted1$station]
      }
      
      # add period
      if ((calStart != "") | (calEnd != "")) {
        melted1$period <- "Validation"
        melted1$period[(as.Date(melted1$DATETIME) >= calStartDate) & 
                        (as.Date(melted1$DATE) <= calEndDate)] <- "Calibration"
      }
      
      
      # if by year, add year and fakedate
      if (byYear) {
        melted1$YEAR <- as.numeric(format(melted1$DATETIME, format = "%Y"))
        melted1$DATETIME <- as.Date(format(melted1$DATE, 
                                          format = "%d-%m-2000 %H:%M"), 
                                   format = "%d-%m-%Y %H:%M")
      }
      
      # filter out measured or simulated if not selected
      if (!meas) {
        melted1 <- melted1[melted1$type != "Measured", ]
      }
      
      if (!sim) {
        melted1 <- melted1[melted1$type != MESHname1, ]
      }
      
      # data set 2
      melted2 <- reshape2::melt(MESHvals2, id.vars = "DATETIME")
      meas_loc2 <- stringr::str_detect(melted2$var, "MEAS")
      meas_pieces2 <- stringr::str_split_fixed(melted2$var[meas_loc2], "QOMEAS", 2)
      meas_vals2 <- meas_pieces2[,2]
      melted2$type <- ""
      melted2$type[meas_loc2] <- "Measured"
      melted2$station <- ""
      melted2$station[meas_loc2] <- as.numeric(meas_vals2)
      
      sim_loc2 <- stringr::str_detect(melted2$var, "SIM")
      melted2$type[sim_loc2] <- MESHname2
      sim_pieces2 <- stringr::str_split_fixed(melted2$var[sim_loc2], "QOSIM", 2)
      sim_vals2 <- sim_pieces2[,2]
      melted2$station[sim_loc2] <- as.numeric(sim_vals2)
      
      # add station name
      if (length(stationNames2) >= numStations2) {
        melted2$station <- stationNames2[melted2$station]
      }
      
      # add period
      if ((calStart != "") | (calEnd != "")) {
        melted2$period <- "Validation"
        melted2$period[(as.Date(melted2$DATETIME) >= calStartDate) & 
                         (as.Date(melted2$DATE) <= calEndDate)] <- "Calibration"
      }
      
      
      # if by year, add year and fakedate
      if (byYear) {
        melted1$YEAR <- as.numeric(format(melted1$DATETIME, format = "%Y"))
        melted1$DATETIME <- as.Date(format(melted1$DATE, 
                                           format = "%d-%m-2000 %H:%M"), 
                                    format = "%d-%m-%Y %H:%M")
      }
      # combine melted data sets
      melted <- rbind(melted1, melted2)
      
      
      # filter out measured or simulated if not selected
      if (!meas) {
        melted1 <- melted1[melted1$type != "Measured", ]
      }
      
      if (!sim) {
        melted1 <- melted1[melted1$type != MESHname1, ]
      }
      
      # do plot
      if (!byYear & byStation) {
        p <- ggplot2::ggplot(melted, ggplot2::aes(DATETIME, value,
                                                  colour = type)) +
          ggplot2::geom_line(alpha = alpha) +
          ggplot2::xlab("") +
          ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                         sep = ""))) 
        return(p)
        
      } else if (!byYear & !byStation) {
        p <- ggplot(melted, ggplot2::aes(DATETIME, value, colour = type)) +
          ggplot2::geom_line(alpha = alpha) +
          ggplot2::xlab("") +
          ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                         sep = ""))) +
          ggplot2::theme(legend.title = ggplot2::element_blank()) +
          ggplot2::scale_x_date(date_labels = "%b")
        return(p)
      } else if (!byStation & byYear) {
        p <- ggplot(melted, ggplot2::aes(DATETIME, value, colour = type)) +
          ggplot2::geom_line(alpha = alpha) +
          ggplot2::xlab("") +
          ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                         sep = ""))) +
          ggplot2::theme(legend.title = ggplot2::element_blank()) +
          ggplot2::scale_x_date(date_labels = "%b")
        return(p)
      } else if (byStation & byYear) {
        p <- ggplot(melted, ggplot2::aes(DATETIME, value, colour = station, 
                                         linetype = type)) +
          ggplot2::geom_line(alpha = alpha) +
          ggplot2::xlab("") +
          ggplot2::ylab(expression(paste("Discharge (m", ""^{3}, "/s)", 
                                         sep = ""))) +
          ggplot2::theme(legend.title = ggplot2::element_blank()) +
          ggplot2::scale_x_date(date_labels = "%b")
      }
  }
}