#' @title Finds record in r2c data specified by string
#'
#' @param recordLines Requred. Vector of lines from r2c file.
#' @param string Required. Record name to searh for.
#' @author Kevin Shook
#' @return Returns trimmed record.

findRecord <- function(recordLines, string) {
  linenum <- grep(string, recordLines, fixed = TRUE)
  record <- recordLines[linenum]
  
  # parse value from record
  pieces <- stringr::str_split_fixed(record, string, n = 2)
  value <- pieces[[2]]
  
  # remove whitespace
  value <- stringr::str_trim(value)
  return(value)
}