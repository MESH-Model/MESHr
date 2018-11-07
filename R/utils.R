#' @title Finds record in r2c data specified by string
#'
#' @param recordLines Requred. Vector of lines from r2c file.
#' @param string Required. Record name to searh for.
#' @param ignore.case Optional. If \code{TRUE} (the default), 
#' then case is ignored.
#' @author Kevin Shook
#' @return Returns trimmed record.

findRecord <- function(recordLines, string, ignore.case = TRUE) {
  if (ignore.case) {
    linenum <- grep(string, recordLines, ignore.case = TRUE, useBytes = TRUE) 
    record <- recordLines[linenum]
    
    pieces <- stringr::str_split_fixed(record, 
                                       stringr::regex(string, 
                                       ignore_case = TRUE),
                                       n = 2)
    value <- pieces[[2]]
  }
  else {
    linenum <- grep(string, recordLines, fixed = TRUE, useBytes = TRUE) 
    # parse value from record
    record <- recordLines[linenum]
    pieces <- stringr::str_split_fixed(record, string, n = 2)
    value <- pieces[[2]]
  }  

  # remove whitespace
  value <- stringr::str_trim(value)
  return(value)
}


#' @title Gets the Windows end of line characters
#'
#' @description Finds the end of line (eol) characters required for writing Windows files, such as CRHM obs files. No parameters are required. This is an internal \pkg{CRHMr} function and should \emph{never} need to be called directly.
#' @return Returns the Windows end of line characters (cr and lf).
#' @export
#' @author Kevin Shook
#' @note This function is used to make the creation of Windows-specific files work on all platforms. CRHM requires its obs and project files to use the Windows end of line characters, which are expressed differently on UNIX-based operating systems such as Linux and OSX.
#' @examples 
#' windowsEndOfLine <- win.eol()
#' 
win.eol <- function(){
  # set line end characters for all OS
  if (stringr::str_detect(.Platform$OS.type, 
                          stringr::fixed('win',ignore_case = TRUE)))
    eol <- '\n'
  else
    eol <- '\r\n'
  
  return(eol)
}


#' @title Parses a string containing numbers
#'
#' @param numString Required. A character string containing numbers separated by any number of spaces.
#'
#' @return Returns a numeric vector.
#' @author Kevin Shook
#' @export
#'
#' @examples
#' parseNums(' 1  2 3   4     5 ')
parseNums <- function(numString){
  # remove padding
  numString <- stringr::str_trim(numString)
  
  # swap tabs for chracters
  numString <- stringr::str_replace_all(numString, stringr::fixed('\t'), ' ' )
  double.spaces <- stringr::str_detect(numString, '  ')
  # replace all double spaces with single spaces
  
  while (double.spaces) {
    numString <- stringr::str_replace_all(numString, '  ', ' ' )
    double.spaces <- stringr::str_detect(numString, '  ')
  }
  
  nums <- as.numeric(unlist(stringr::str_split(numString, ' ')))
  return(nums)
}


#' @title Parses a string containing several sub-strings
#' @param textString Required. A character string containing strings separated by any number of spaces.
#'
#' @return Returns a character vector.
#' @author Kevin Shook
#' @export
#'
#' @examples
#' parseText(' red  green    blue      black')
parseText <- function(textString){
  #
  # returns a vector
  
  # remove padding
  textString <- stringr::str_trim(textString)
  
  # swap tabs for chracters
  textString <- stringr::str_replace_all(textString, stringr::fixed('\t'), ' ' )
  double.spaces <- stringr::str_detect(textString, '  ')
  # replace all double spaces with single spaces
  
  while (double.spaces) {
    textString <- stringr::str_replace_all(textString, '  ', ' ' )
    double.spaces <- stringr::str_detect(textString, '  ')
  }
  
  texts <- unlist(stringr::str_split(textString, ' '))
  return(texts)
}

#' @title Find if variable is present in a data frame

#' @param dataframe Required. Dataframe to be checked
#' @param variable Required. Variable name as a character string.
#'
#' @return Returns \code{TRUE} or \code{FALSE}
#' @export
#'
#' @examples \dontrun{var_present(values, "datetime")}
var_present <- function(dataframe, variable) {
 result <- variable %in% names(dataframe)
 return(result)
}
