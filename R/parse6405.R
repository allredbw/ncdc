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
  file6405 <- retrieveasos(call, yearmonth, file=6405, force=force)
  if(is.na(file6405)) {
    warn.msg <- paste("ASOS 6405 (wind) one minute data for", call, 
                      "station, year", year, "month", month, "is unavailable.")
    warning(warn.msg)
    return(NA)
  }
  lines6405 <- readLines(file6405, warn=F)
  
  ## remove brackets
  #lines6405 <- gsub("\\[", " ", lines6405)
  #lines6405 <- gsub("]", " ", lines6405)
  
  ## use regular expression to extract wind data
  wind.pattern  <- paste(" ",
                         "((M[[:space:]]{1,8})|([0-9]{1,3}[[:space:]]+))",
                         "((M[[:space:]]{1,8})|([0-9]{1,3}[[:space:]]+))",
                         "((M[[:space:]]+)|([0-9]{1,3}[[:space:]]+))",
                         "((M[[:space:]]+)|([0-9]{1,3}[[:space:]]{1}))", 
                         sep="")
  
  wind <- lapply(lines6405, function(x) {
    windexpr <- regexpr(text=x, pattern=wind.pattern)
    if(windexpr>0) {
      wind.vec <- unlist(strsplit(substr(x, start=windexpr+1, 
                                         stop=windexpr+attr(windexpr, 
                                                            "match.length")),
                                  " +"))
      ## replace Ms with NA
      wind.vec[wind.vec=="M"] <- NA
      return(wind.vec)
      #if(length(wind.vec)==4) return(wind.vec) else return(c(NA, NA, NA, NA))
    } else c(NA, NA, NA, NA)
  })
  
  ## two minute wind direction
  D360 <- sapply(wind, function(x) as.integer(x[1]))
  ## two minute wind speed (knots)
  M2SP <- sapply(wind, function(x) as.integer(x[2]))
  ## direction of maximum (5 second average) wind during previous minute
  MXD5 <- sapply(wind, function(x) as.integer(x[3]))
  ## speed of maximum (5 second average) wind during previous mintue (knots)
  MXS5 <- sapply(wind, function(x) as.integer(x[4]))
  
  ## use regular expression to extract time
  time.pattern <- " [A-Z]{3}\\d+"
  ## utc offset
  utcoffset <- asosstns$UTC[match(substr(call, 2, 4), asosstns$CALL)]
  ## use sprintf() to convert to character, "-0000" or "+0000"
  utcoffset <- sprintf("%+03d", utcoffset)
  
  timeexpr <- regexpr(text=lines6405, pattern=time.pattern)
  TIME <- substr(lines6405, start=timeexpr+4, stop=timeexpr+15)
  TIME <- as.POSIXct(paste(TIME, utcoffset, "00", sep=""), 
                     format="%Y%m%d%H%M%z")
  
 ## return as data frame
  return(data.frame(CALL=call, TIME, D360, M2SP, MXD5, MXS5))
}