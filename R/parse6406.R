parse6406 <- function(call, yearmonth) {
  ## Parse 6406 file; convert to usable dataframe
  ## Arguments:
  ##  call: four letter call sign
  ##  yearmonth: character string indicating year, month; e.g. Jan 2012: 201201
  ## Returns: dataframe containing call, timestamp, 
  ## temperature, dewpoint, pressure, and precipitation from 6406 file
  
  ## extract year from yearmonth
  year <- substr(x=yearmonth, start=1, stop=4)
  month <- substr(x=yearmonth, start=5, stop=6)
  
  ## read 6406 file
  file6406 <- retrieve6406(call, yearmonth)
  if(is.na(file6406)) {
    warn.msg <- paste("ASOS one minute data for", call, "station, year",
                      year, "month", month, "is unavailable.")
    warning(warn.msg)
    return(NA)
  }
  lines6406 <- readLines(file6406, warn=F)
  
  ## remove brackets
  lines6406 <- gsub("\\[", " ", lines6406)
  lines6406 <- gsub("]", " ", lines6406)
  ## split into list at whitespaces; treat multiple whitespaces as one
  list6406 <- strsplit(lines6406, " +")
  ## convert each list element to a data frame
  list6406 <- lapply(list6406, function(x) {x <- as.data.frame(t(x), 
                                                               stringsAsFactors=F)
                                            colnames(x) <- 1:length(x)
                                            x[x=="M"] <- NA
                                            return(x)})
  ## some 6406 files contain a filed with '0000' or similar;
  ## this field is unidentified
  ## replace with NA
  list6406 <- lapply(list6406, function(x) {if(any(grepl(x=x, 
                                                         pattern="^000+")==T)){ 
    x[, grep(x=x, 
             pattern="^000+")] <- NA
    return(x)} else x})
  ## temperature (dry bulb), Fahrenheit
  TMPD <- sapply(list6406, function(x) {last <- length(x)
                                        return(as.integer(x[, last-1]))})
  ## dew-point temperature, Fahrenheit
  DPTP <- sapply(list6406, function(x) {last <- length(x) 
                                        return(as.integer(x[, last]))})
  ## precipitation, inches
  PCPN <- sapply(list6406, function(x) {if(length(x)>10) {
    return(as.numeric(x[, 5]))} else {
      return(as.numeric(x[, 4]))}})
  ## pressure sensor 1, inches of mecury (Hg)
  PRS1 <- sapply(list6406, function(x) {last <- length(x)
                                        return(as.numeric(x[, last-2]))})
  ## pressure sensor 2, inches of mecury (Hg)
  PRS2 <- sapply(list6406, function(x) {last <- length(x)
                                        return(as.numeric(x[, last-3]))})
  ## pressure sensor 3, inches of mecury (Hg)
  PRS3 <- sapply(list6406, function(x) {last <- length(x)
                                        return(as.numeric(x[, last-4]))})
  ## time
  TIME <- sapply(list6406, function(x) {substr(x[, 2], 4, 15)})
  ## get UTC offset from asos.stns
  utcoffset <- asos.stns$UTC[match(substr(call, 2, 4), asos.stns$CALL)]
  ## use sprintf() to convert to character, "-0000" or "+0000"
  utcoffset <- sprintf("%03+d", utcoffset)
  TIME <- as.POSIXct(paste(TIME, utcoffset, "00", sep=""), 
                     format="%Y%m%d%H%M%z")
  ## return as data frame
  return(data.frame(CALL=call, TIME, TMPD, DPTP, PRS1, PRS2, PRS3, PCPN))
}