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