# Finds record in r2c data specified by string
findRecord <- function(recordLines, string, ignore.case = TRUE) {
  if (ignore.case) {
    linenum <- grep(string, recordLines, ignore.case = TRUE) 
    record <- recordLines[linenum]
    
    pieces <- stringr::str_split_fixed(record, 
                                       stringr::regex(string, 
                                       ignore_case = TRUE),
                                       n = 2)
    value <- pieces[[2]]
  }
  else {
    linenum <- grep(string, recordLines, fixed = TRUE) 
    # parse value from record
    record <- recordLines[linenum]
    pieces <- stringr::str_split_fixed(record, string, n = 2)
    value <- pieces[[2]]
  }  


  # remove whitespace
  value <- stringr::str_trim(value)
  return(value)
}


# Gets the Windows end of line characters
win.eol <- function(){
  # set line end characters for all OS
  if (stringr::str_detect(.Platform$OS.type, 
                          stringr::fixed('win',ignore_case = TRUE)))
    eol <- '\n'
  else
    eol <- '\r\n'
  
  return(eol)
}


# Parses a string containing numbers
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


# Parses a string containing several sub-strings
parseText <- function(textString){
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

# see if variable is present in a data frame
var_present <- function(dataframe, variable) {
 result <-  variable %in% names(dataframe)
 return(result)
}
