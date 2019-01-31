read_MESH_binary_grid <- function(binaryFile, shedFile) {
  
  # get watershed grid definition
  shed <- read_r2c_shed(shedFile, as_rasters = FALSE)
  shed_locs <- shed[,,1]    # get basin point locations
  
  # get row and cols of shed_locs
  shed_rows_cols <- which(shed_locs > 0, arr.ind = TRUE)
  shed_rows_cols <- as.matrix(shed_rows_cols)
  
  # add values
  shed_rows_cols <- cbind(shed_rows_cols, shed_locs[shed_rows_cols])
  names(shed_rows_cols) <- c( "row", "col", "loc")
  num_vals <- nrow(shed_rows_cols)
  
  # order by value
  shed_rows_cols <- shed_rows_cols[order(shed_rows_cols$loc),]
  
  # now read in binary file
  binary <- file(binaryFile, "rb")
  year <- c(0)
  month <- c(0)
  day <- c(0)
  hour <- c(0)
  min <- c(0)
  all_frame_vals <- matrix[]
  done <- FALSE
  while (!done) {
    header <- readBin(binary, integer(), 9)
    if (length(header) == 0) {
      record_num <- header[3]
      year[record_num] <- header[4]
      month[record_num] <- header[5]
      day[record_num] <- header[6]
      hour[record_num] <- header[7]
      min[record_num] <- header[8]
      
      record_length <-  readBin(binary, integer(), 1)
      frame_vals <- readBin(binary, numeric(), n = num_vals, size = 4)
      record_length <-  readBin(binary, integer(), 1)
      frame_matrix <- data.matrix(frame_vals)
      all_frame_vals[,,record_num] <- frame_matrix
    } else {
      done <- TRUE
    }
  }

  close(binaryFile)
  
  return(all_frame_vals)
  
}