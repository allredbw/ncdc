parse6405 <- function(call, yearmonth, force=F) {
  ## Parse 6405 file; convert to usable dataframe
  ## Arguments:
  ##  call: four letter call sign
  ##  yearmonth: character string indicating year, month; e.g. Jan 2012: 201201
  ## Returns: dataframe containing call, timestamp, 
  ## two minute average wind direction, two minute average wind speed (knots),
  ## direction of maximum (five second average) wind during previous minute,
  ## and speed of maximum (five second average) wind during previous minute 
  ## (knots).
  ## See http://www1.ncdc.noaa.gov/pub/data/documentlibrary/tddoc/td3285.pdf
  ## for descriptions.
  
  ## extract year from yearmonth
  year <- substr(x=yearmonth, start=1, stop=4)
  month <- substr(x=yearmonth, start=5, stop=6)
  
  ## read 6405 file
  file6405 <- retrieve6405(call, yearmonth, force=force)
  if(is.na(file6405)) {
    warn.msg <- paste("ASOS 6405 (wind) one minute data for", call, 
                      "station, year", year, "month", month, "is unavailable.")
    warning(warn.msg)
    return(NA)
  }
  lines6405 <- readLines(file6405, warn=F)
  
  ## remove brackets
  lines6405 <- gsub("\\[", " ", lines6405)
  lines6405 <- gsub("]", " ", lines6405)
  ## split into list at whitespaces; treat multiple whitespaces as one
  list6405 <- strsplit(lines6405, " +")
  ## convert each list element to a data frame
  list6405 <- lapply(list6405, function(x) {x <- as.data.frame(t(x), 
                                                               stringsAsFactors=F)
                                            colnames(x) <- 1:length(x)
                                            x[x=="M"] <- NA
                                            return(x)})
  ##records with columns < 8 set as NA
#   list6405 <- lapply(list6405,function(x) {if(length(x)<8) return(NA)
#                                            else return(x)})
  
  ## runway visual regular expression
  vis.pattern <- "(^\\d{1,2}?[A-Z])|(\\+$)"
  
  ## two minute wind direction
  D360 <- sapply(list6405, function(x) { if(length(x)>=8) {
    if(any(grepl(x, pattern=vis.pattern))) last <- length(x)-1
    else last <- length(x)
    return(as.integer(x[, last-3]))
  } else return(NA)})
  ## two minute wind speed (knots)
  M2SP <- sapply(list6405, function(x) { if(length(x)>=8) {
    if(any(grepl(x, pattern=vis.pattern))) last <- length(x)-1
    else last <- length(x)
    return(as.integer(x[, last-2]))
  } else return(NA)})
  ## direction of maximum (5 second average) wind during previous minute
  MXD5 <- sapply(list6405, function(x) { if(length(x)>=8) {
    if(any(grepl(x, pattern=vis.pattern))) last <- length(x)-1
    else last <- length(x)
    return(as.integer(x[, last-1]))
  } else return(NA)})
  ## speed of maximum (5 second average) wind during previous mintue (knots)
  MXS5 <- sapply(list6405, function(x) { if(length(x)>=8) {
    if(any(grepl(x, pattern=vis.pattern))) last <- length(x)-1
    else last <- length(x)
    return(as.integer(x[, last]))
  } else return(NA)})
  ## time
  TIME <- sapply(list6405, function(x) {substr(x[, 2], 4, 15)})
  ## get UTC offset from asosstns
  utcoffset <- asosstns$UTC[match(substr(call, 2, 4), asosstns$CALL)]
  ## use sprintf() to convert to character, "-0000" or "+0000"
  utcoffset <- sprintf("%03+d", utcoffset)
  TIME <- as.POSIXct(paste(TIME, utcoffset, "00", sep=""), 
                     format="%Y%m%d%H%M%z")
  ## return as data frame
  return(data.frame(CALL=call, TIME, D360, M2SP, MXD5, MXS5))
}