#' Reads in a ts0 file
#' 
#' @description Reads in a ts0 file. The time series values, if present, will be
#' stored in a data frame. Optionally, the meta data will be stored as lists. 
#' The meta data are of 2 types, the header values, which refer to the entire 
#' file and column values, which pertain to individual columns.
#'
#' @param tb0File Required. The name of the file to be read.
#' @param values_only Optional. If \code{TRUE}, only the time series values will
#' be returned. If {FALSE}, the meta data will also be returned. Note that if
#' the value is set to \code{TRUE}, and there are no time series values in the
#' file, as for a reservoir file, then an error will result.
#' @param timezone Optional. The data time series have \code{POSIXct} datetime
#' values. You may want to specify the \code{timezone} of the data.
#' If the \code{timezone} is not specified, your default value (i.e. your
#' time zone) will be used.
#' @param NAvalue Optional. If specified, values smaller than \code{NAvalue}
#' will be set to \code{NA_real_}
#'
#' @return Returns time series as a data frame. If meta data are specified, they
#' are returned as lists.
#' 
#' @author Kevin Shook
#' @seealso \code{\link{r2c2raster}} \code{\link{readOutputTimeseriesCSV}} 
#' @export
#'
#'
#' @examples \dontrun{
#' qvals <- tb02dataframe("MESH_input_streamflow.tb0", NAvalue = -0.01, values_only = TRUE)
#' }
tb02dataframe <- function(tb0File = "",  values_only = TRUE, timezone = "", NAvalue = NULL) {
  if (tb0File == "") {
    cat("Error: missing tb0 basin file\n")
    return(FALSE)
  }
  
  # read in file
  con <- file(tb0File, "r", blocking = FALSE, encoding = "ISO_8859-2")
  tb0 <- readLines(con)
  close(con)
  
  end_header <-  grep(":endHeader", x = tb0, ignore.case = TRUE)
  data_lines <- tb0[-(1:end_header)]
  header_lines <- tb0[1:(end_header - 1)]
  
  # get meta data from header
  filetype <- findRecord(header_lines, ":FileType")
  datatype <- findRecord(header_lines, "DataType")
  application <- findRecord(header_lines, ":Application")
  written_by <- findRecord(header_lines, ":WrittenBy")
  creation_date <- findRecord(header_lines, ":CreationDate")
  
  delta_t <- as.numeric(findRecord(header_lines, ":DeltaT"))
  name <- findRecord(header_lines, ":Name")
  projection <- findRecord(header_lines, ":Projection")
  ellipsoid <- findRecord(header_lines, ":Ellipsoid")
  
  # look for attributes which may or may not be present
  start_time <- findRecord(header_lines, ":StartTime")
  
  # check to see if the date is specified, use it if it is
  date_present <- sum(stringr::str_detect(header_lines, 
                                      stringr::regex(":StartDate", 
                                                     ignore_case = TRUE)))
  if (date_present > 0) {
    start_date <- findRecord(header_lines, ":StartDate")
    start_time <- paste(start_date, start_time)
  }
  
  source_file_present <-  sum(stringr::str_detect(header_lines, 
                                    stringr::regex(":UnitConversion", 
                                    ignore_case = TRUE)))
  if (source_file_present)
    source_file <- findRecord(header_lines, ":SourceFile")
  
  unit_conversion_present <- sum(stringr::str_detect(header_lines, 
                                              stringr::regex(":UnitConversion", 
                                                         ignore_case = TRUE)))
  if (unit_conversion_present)     
    unit_conversion <- findRecord(header_lines, ":UnitConversion")
  
  attributeunits_present <- sum(stringr::str_detect(header_lines, 
                                              stringr::regex(":AttributeUnits", 
                                                          ignore_case = TRUE)))
  if (attributeunits_present)     
    attributeunits <- findRecord(header_lines, ":AttributeUnits")
  
  RoutingDeltaT_present <- sum(stringr::str_detect(header_lines, 
                                                    stringr::regex(":RoutingDeltaT", 
                                                                   ignore_case = TRUE)))
  if (RoutingDeltaT_present)     
    routingdeltat <- findRecord(header_lines, ":RoutingDeltaT") 
  
  
   # get column meta data  
  column_start_line <- grep(":ColumnMetaData", stringr::regex(header_lines, ignore_case = TRUE)) + 1
  column_end_line <- grep(":EndColumnMetaData",  stringr::regex(header_lines, ignore_case = TRUE)) - 1
  column_meta_data <- header_lines[column_start_line:column_end_line]
  
  columnNames <-  parseText(findRecord(column_meta_data, ":ColumnName"))
  columnUnits <-  parseText(findRecord(column_meta_data, ":ColumnUnits"))
  columnType <-  parseText(findRecord(column_meta_data, ":ColumnType"))
  columnLocationX <- parseNums(findRecord(column_meta_data, ":ColumnLocationX"))
  columnLocationY <- parseNums(findRecord(column_meta_data, ":ColumnLocationY"))
  
  # see if the drainage area is present, use it if it is
  da_present <- sum(stringr::str_detect(header_lines, 
                                          stringr::regex("#ApproxDA", 
                                                         ignore_case = TRUE)))
  if (da_present > 0) {
    DA <-  parseNums(findRecord(column_meta_data, "#ApproxDA"))
  }
  
  # look for coeffs
  coeff1_present <- sum(stringr::str_detect(header_lines, 
                                        stringr::regex(":coeff1", 
                                                       ignore_case = TRUE)))
  if (coeff1_present) {
    coeff1 <-  parseNums(findRecord(column_meta_data, ":coeff1"))
  }
  
  coeff2_present <- sum(stringr::str_detect(header_lines, 
                                            stringr::regex(":coeff2", 
                                                       ignore_case = TRUE)))
  if (coeff2_present) {
    coeff2 <-  parseNums(findRecord(column_meta_data, ":coeff2"))
    
  }
  coeff3_present <- sum(stringr::str_detect(header_lines, 
                                            stringr::regex(":coeff3", 
                                                      ignore_case = TRUE)))
  if (coeff3_present) {
    coeff3 <-  parseNums(findRecord(column_meta_data, ":coeff3"))
  }
  
  coeff4_present <- sum(stringr::str_detect(header_lines, 
                                            stringr::regex(":coeff4", 
                                                        ignore_case = TRUE)))
  if (coeff4_present) {
    coeff4 <-  parseNums(findRecord(column_meta_data, ":coeff4"))
  }
  
  coeff5_present <- sum(stringr::str_detect(header_lines, 
                                            stringr::regex(":coeff5", 
                                                        ignore_case = TRUE)))
  if (coeff5_present) {
    coeff5 <-  parseNums(findRecord(column_meta_data, ":coeff5"))
  }
  
  value1_present <- sum(stringr::str_detect(header_lines, 
                                            stringr::regex(":value1", 
                                                           ignore_case = TRUE)))
  if (value1_present) {
    value1 <-  parseNums(findRecord(column_meta_data, ":value1"))
  }
  
  
  
  # now read in data - if present
  if (length(data_lines) > 0) {
    values <- read.table(textConnection(data_lines), header = FALSE, 
                         stringsAsFactors = FALSE)
    
    # add titles
    names(values) <- columnNames
    num_values <- nrow(values)
    
    # add datetime
    start_datetime <- as.POSIXct(start_time, format = "%Y/%m/%d %H:%M:%S")
    
    # create time series
    time_step <- delta_t   # hours
    end_datetime <- start_datetime + (time_step * 3600) * (num_values - 1)
    datetime <- seq(from = start_datetime, 
                    to = end_datetime, 
                    by = (3600 * time_step))  # to seconds
    
    data_vals <- cbind(datetime, values)
    if (!is.null(NAvalue)) {
      data_vals[data_vals < NAvalue] <- NA_real_
    }
  }
  
  if (!values_only) {
    # assemble meta data
    header_meta <- list(
      "filetype" = filetype,
      "datatype" = datatype,
      "application" = application,
      "written_by" = written_by,
      "creation_date" = creation_date,
      "delta_t" = delta_t,
      "name" = name,
      "projection" = projection,
      "ellipsoid" = ellipsoid,
      "start_time" = start_time
    )
    
    # add variable meta data
    if (source_file_present) 
      header_meta["source_file"] <- source_file
    
    if (unit_conversion_present) 
      header_meta["unit_conversion"] <- unit_conversion

    if (attributeunits_present) 
      header_meta["attributeunits"] <- attributeunits  
    
    if (RoutingDeltaT_present) 
      header_meta["routingdeltat"] <- routingdeltat 
    
    # first do always-present columns
    column_meta_data <- data.frame(columnNames, 
                                   columnUnits, 
                                   columnType,
                                   columnLocationX,
                                   columnLocationY,
                            stringsAsFactors = FALSE)
    
    # add variable columns
    if (da_present)
      column_meta_data <- cbind(column_meta_data, DA)
    if (coeff1_present)
      column_meta_data <- cbind(column_meta_data, coeff1)
    if (coeff2_present)
      column_meta_data <- cbind(column_meta_data, coeff2)
    if (coeff3_present)
      column_meta_data <- cbind(column_meta_data, coeff3)
    if (coeff4_present)
      column_meta_data <- cbind(column_meta_data, coeff4)
    if (coeff5_present)
      column_meta_data <- cbind(column_meta_data, coeff5)
    if (value1_present)
      column_meta_data <- cbind(column_meta_data, value1)
    
    output_vals <- list(header_meta = header_meta, 
                        column_meta_data = column_meta_data)
    
    if (length(data_lines) > 0)
      output_vals[["data"]] <- data_vals
    
    return(output_vals)
  } else {
    if (length(data_lines) <= 0) {
      cat("Error: no time series values in tb0 basin file\n")
      return(FALSE)
    }
    else
      return(data_vals)
  }
}