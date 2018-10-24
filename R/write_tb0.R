#' Writes a MESH tb0 file
#'
#' @param values Optional. A data frame of the values to be written, if the file
#' is to be a time seired. Note that the first column must be called 
#' \code{datetime} and must be a POSIXct date/time.
#' @param column_meta Required. A data frame containing the following columns
#' #' \describe{
#'  \item{columnUnits}{required}
#'  \item{columnType}{required}
#'  \item{columnName}{required}
#'  \item{columnLocationX}{required}
#'  \item{columnLocationY}{required}
#'  \item{DA}{optional}
#'  \item{coeff1}{optional}
#'  \item{coeff2}{optional}
#'  \item{coeff3}{optional}
#'  \item{coeff4}{optional}
#'  \item{coeff5}{optional}
#' }
#' @param header Required. A list containing the following variables
#'  \describe{
#'  \item{filetype}{optional, default is \code{tb0  ASCII  EnSim 1.0}}
#'  \item{datatype}{optional, default is \code{Time Series}}
#'  \item{application}{optional, default is \code{EnSimHydrologic}}
#'  \item{version}{optional, default is default is \code{2.1.23}}
#'  \item{written_by}{optional, default is default is \code{MESHr}}
#'  \item{creation_date}{optional, default is current date/time}
#'  \item{source_file}{optional, default is nothing}
#'  \item{name}{required}
#'  \item{projection}{required, character string}
#'  \item{ellipsoid}{required, character string}
#'  \item{start_time}{required if \code{values} are not specified}
#'  \item{delta_t}{required if \code{values} are not specified}
#'  \item{attributeunits}{optional, default is nothing}
#'  \item{unitconversion}{optional, default is nothing}
#' }
#' @param NAvalue Optional. Value to be used for \code{NA_real_} values in
#' the \code{.tb0} file. The default value is -1, which is not suitable
#' for air temperatures.
#' @param tb0File Required. Name of file to be written.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns 
#' \code{FALSE}
#' @author Kevin Shook
#' @seealso \code{\link{read_tb0}}
#' @export
#'
#' @examples \dontrun{ write_tb0(values, column_meta_data, header, "MESH_values.ts0")
#' }
write_tb0 <- function(values = NULL, column_meta = NULL, header = NULL, 
                      NAvalue = -1,
                      tb0File = "") {
  if (tb0File == "") {
    cat("Error: missing tb0 file\n")
    return(FALSE)
  }
  
  if (is.null(column_meta)) {
    cat("Error: missing tb0 column meta data\n")
    return(FALSE)
  }
  
  if (is.null(header)) {
    cat("Error: missing tb0 header\n")
    return(FALSE)
  }

  if (!is.null(values)) {
    if (ncol(column_meta) != ncol(column_meta)) {
      cat("Error: column meta data and values have different numbers of columns\n")
      return(FALSE)
    }
  }

  # find eol characters to use
  eol <- win.eol()
  
  # write header to file
  # first, write header without specified data 
  # bulid up vector of header lines based on provided data and/or 
  # default values
  
  header1 <- "########################################"
  if (var_present(header, "filetype")) {
    if (header$filetype >= "")
      filetype <- paste(":FileType ", header$filetype, sep = "") 
    else
      filetype <- ":FileType   tb0 ASCII   GreenKenue  1"
  } else {
    filetype <- ":FileType   tb0 ASCII   GreenKenue  1"
  }
 
  header1 <- c(header1, filetype, "#")
  
  if (var_present(header, "datatype")) {
    if (header$datatype != "")
      datatype <- paste("#   DataType ", header$filetype, sep = "") 
    else
      datatype <- "#   DataType    GreenKenue  Table"
  } else {
    datatype <- "#   DataType    GreenKenue  Table"
  }
  
  header1 <- c(header1, datatype, "#")
  
  if (var_present(header, "application")) {
    if (header$application != "")
      application <- paste(":Application ", header$application, sep = "") 
    else
      application <- ":Application    GreenKenue  "
  } else {
    application <- ":Application    GreenKenue  "
  }
  
  header1 <- c(header1, application)
  
  if (var_present(header, "version")) {
    if (header$version != "")
      version <- paste(":Version ", header$version, sep = "") 
    else
      version <- ":Version    2.1.23"
  } else {
    version <- ":Version    2.1.23"
  }
  
  header1 <- c(header1, version)
  
  if (var_present(header, "written_by")) {
    if (header$written_by != "")
      written_by <- paste(":WrittenBy ", header$written_by, sep = "") 
    else
      written_by <- ":WrittenBy    MESHr"
  } else {
    written_by <- ":WrittenBy    MESHr"
  }
  
  header1 <- c(header1, written_by)
  
  if (var_present(header, "creation_date")) {
    if (header$creation_date != "")
      creation_date <- paste(":CreationDate ", header$creation_date, sep = "") 
    else
      creation_date <- paste(":CreationDate ", date(), sep = "")
  } else {
    creation_date <- paste(":CreationDate ", date(), sep = "")
  }
  
  header1 <- c(header1, creation_date, "#",
               "#---------------------------------------")
  
  # create variable line header
  header2 <- "#"
  
  if (var_present(header, "source_file")) {
    if (header$source_file != "") {
      source_file <- paste(":SourceFile ", header$source_file, sep = "")
      header2 <- c(header2, source_file, "#")
    }
  }
    

  if (var_present(header, "name")) {
    if (header$name != "") {
      name <- paste(":Name ", header$name, sep = "")
    }
    else {
      cat("Error: name is missing\n")
      return(FALSE)
    }
  } else {
    cat("Error: name must be specified\n")
    return(FALSE)
  }

  header2 <- c(header2, name, "#")

  if (var_present(header, "projection")) {
    if (header$projection != "") {
      projection <- paste(":Projection ", header$projection, sep = "")
    }
    else {
      cat("Error: projection is missing\n")
      return(FALSE)
    }
  } else {
    cat("Error: projection must be specified\n")
    return(FALSE)
  }
  
  header2 <- c(header2, projection)
  
  if (var_present(header, "ellipsoid")) {
    if (header$ellipsoid != "") {
      ellipsoid <- paste(":Ellipsoid ", header$ellipsoid, sep = "")
    }
    else {
      cat("Error: ellipsoid is missing\n")
      return(FALSE)
    }
  } else {
    cat("Error: ellipsoid must be specified\n")
    return(FALSE)
  }
               
  header2 <- c(header2, ellipsoid)
  
  if (var_present(header, "start_time")) {
    start_time <- paste(":StartTime ", header$start_time, sep = "")
  } else {
    if (!is.null(values)) {
      if (names(values)[1] == "datetime") {
        start_time <- paste(":StartTime ", format(values$datetime[1],
                                                  format = "%Y/%m/%d %H:%M:%S.0", 
                                                  sep = ""))
      } else {
        cat("Error: start_time must be specified or data must be time series\n")
        return(FALSE)
      }
    }
  }
  
  header2 <- c(header2, start_time, "#")
  
  if (var_present(header, "attributeunits")) {
    attributeunits <- paste(":AttributeUnits ", header$attributeunits, sep = "")
    header2 <- c(header2, attributeunits)
  } 

  if (var_present(header, "unitconversion")) {
    unitconversion <- paste(":UnitConversion ", header$unitconversion, sep = "")
    header2 <- c(header2, unitconversion)
  } 
  
                 
  if (var_present(header, "delta_t")) {
    delta_t <- paste(":DeltaT ", header$delta_t, sep = "")
  } else {
    if (!is.null(values)) {
      if (names(values)[1] == "datetime") {
        dt  <- difftime(values$datetime[2], values$datetime[1], units = "hours")
        dt <- abs(as.numeric(dt))
        delta_t <- paste(":Delta ", dt, sep = "")
      } else {
        cat("Error: delta_t must be specified or data must be time series\n")
        return(FALSE)
      }
    } 
  }
  
  header2 <- c(header2, delta_t)
  
  if (var_present(header, "routingdeltat")) {
    routingdeltat <- paste(":RoutingDeltaT ", header$routingdeltat, sep = "")
    header2 <- c(header2, routingdeltat)
  } 

  header2 <- c(header2, "#")

  
  # now do column meta data

  column_meta_data <- ":ColumnMetaData"
  column_units <- paste(":ColumnUnits ", paste(column_meta$columnUnits, 
                        collapse = " "))
  column_meta_data <- c(column_meta_data, column_units)
  
  column_type <- paste(":ColumnType ", paste(column_meta$columnType, 
                        collapse = " "))
  column_meta_data <- c(column_meta_data, column_type)
  
  column_name <- paste(":ColumnName ", paste(column_meta$columnName,
                        collapse = " "))
  column_meta_data <- c(column_meta_data, column_name)
  
  if (var_present(column_meta, "DA")) {
    column_DA <- paste("#ApproxDA ", paste(column_meta$DA,
                         collapse = " "))
    column_meta_data <- c(column_meta_data, column_DA)
  }
  
  column_locationX <- paste(":ColumnLocationX ", paste(column_meta$columnLocationX, 
                        collapse = " "))
  column_meta_data <- c(column_meta_data, column_locationX)
  
  column_locationY <- paste(":ColumnLocationY ", paste(column_meta$columnLocationY, 
                            collapse = " "))
  column_meta_data <- c(column_meta_data, column_locationY)
  
  if (var_present(column_meta, "coeff1")) {
    column_coeff1 <- paste(":coeff1 ", paste(column_meta$coeff1, 
                       collapse = " "))
    column_meta_data <- c(column_meta_data, column_coeff1)
  }
  
  if (var_present(column_meta, "coeff2")) {
    column_coeff2 <- paste(":coeff2 ", paste(column_meta$coeff2,  
                           collapse = " "))
    column_meta_data <- c(column_meta_data, column_coeff2)
  }
  
  if (var_present(column_meta, "coeff3")) {
    column_coeff3 <- paste(":coeff3 ", paste(column_meta$coeff3, 
                           collapse = " "))
    column_meta_data <- c(column_meta_data, column_coeff3)
  }
    
  if (var_present(column_meta, "coeff4")) {
    column_coeff4 <- paste(":coeff4 ", paste(column_meta$coeff4, 
                           collapse = " "))
    column_meta_data <- c(column_meta_data, column_coeff4)
  }  
    
  if (var_present(column_meta, "coeff5")) {
    column_coeff5 <- paste(":coeff5 ", paste(column_meta$coeff5, 
                           collapse = " "))
    column_meta_data <- c(column_meta_data, column_coeff5)
  }  
  
  if (var_present(column_meta, "value1")) {
    column_value1 <- paste(":value1 ", paste(column_meta$value1, 
                                             collapse = " "))
    column_meta_data <- c(column_meta_data, column_value1)
  } 
  
  column_meta_data <- c(column_meta_data, ":EndColumnMetaData", ":EndHeader	")
  
  # write header and meta data
  con <- file(tb0File, open = "w")
  writeLines(header1, con = con, sep = eol)
  writeLines(header2, con = con, sep = eol)
  writeLines(column_meta_data, con = con, sep = eol)
  close(con)
  
  if (is.null(values)) {
    return(TRUE)
  } else {
    # write values to file
    values <- values[, -1]  # stip datetime
    values[is.na(values)] <- NAvalue
    utils::write.table(values, file = tb0File, append = TRUE,
                row.names = FALSE, col.names = FALSE,
                sep = "  ", eol = eol)
    return(TRUE)
  }
  
}