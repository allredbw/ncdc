parse6406 <- function(call, yearmonth, force=F) {
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
  file6406 <- retrieve6406(call, yearmonth, force=force)
  if(is.na(file6406)) {
    warn.msg <- paste("ASOS 6406 one minute data for", call, "station, year",
                      year, "month", month, "is unavailable.")
    warning(warn.msg)
    return(NA)
  }
  ## read in 6406 file
  lines6406 <- readLines(file6406, warn=F)
  
  ## remove brackets
  lines6406 <- gsub("\\[", " ", lines6406)
  lines6406 <- gsub("]", " ", lines6406)
  ## split into list at whitespaces; treat multiple whitespaces as one
  list6406 <- strsplit(lines6406, " +")
  ## remove Ms
  list6406 <- lapply(list6406, function(x) {
    x[which(x=="M")] <- NA
    return(x)})
  
  ## find pressure positions
  pres.pos <- lapply(list6406, FUN=grep, pattern="^[2-3]{1}\\d{1}\\.\\d{3}$")
  
  ## pressure sensors, inches of mecury (Hg)
  PRES <- mapply(list6406, pres.pos, FUN=function(x, y) {
    if(length(y)==0) {
      return(NULL)
    }
    if(length(y)==1) {
      PRS1 <- as.numeric(x[y[1]])
      return(data.frame(PRS1))
    }
    if(length(y)==2) {
      PRS1 <- as.numeric(x[y[1]])
      PRS2 <- as.numeric(x[y[2]])
      return(data.frame(PRS1, PRS2))
    }
    if(length(y)==3) {
      PRS1 <- as.numeric(x[y[1]])
      PRS2 <- as.numeric(x[y[2]])
      PRS3 <- as.numeric(x[y[3]])
      return(data.frame(PRS1, PRS2, PRS3))
    }
  }, SIMPLIFY=F)
  
  ## extract pressure values from PRES list
  PRS1 <- sapply(PRES, function(x) if(length(x)>=1) x$PRS1 else NA)
  PRS2 <- sapply(PRES, function(x) if(length(x)>=2) x$PRS2 else NA)
  PRS3 <- sapply(PRES, function(x) if(length(x)>=3) x$PRS3 else NA)
  
  ## find precipitation positions
  prec.pos <- lapply(list6406, FUN=grep, pattern="^\\d\\.\\d{2}$")
  
  ## precipitatio, inches
  PCPN <- mapply(list6406, prec.pos, FUN=function(x, y) {
    if(length(y)==0) return(NA)
    else as.numeric(x[y[1]])
  })
  
  ## find temperature positions
  temp.pos <- lapply(list6406, FUN=grep, pattern="^(-)?\\d{0,2}?\\d{1}$")
  
  ## temperature (dry bulb), Fahrenheit
  TMPD <- mapply(list6406, temp.pos, FUN=function(x, y) {
    if(length(y)<2) return(NA)
    else return(as.integer(x[y[1]]))
  })
  
  ## dew-point temperature, Fahrenheit
  DPTP <- mapply(list6406, temp.pos, FUN=function(x, y) {
    if(length(y)<2) return(NA)
    else return(as.integer(x[y[2]]))
  })
  
  ## time
  TIME <- sapply(list6406, function(x) {substr(x[2], 4, 15)})
  ## get UTC offset from asosstns
  utcoffset <- asosstns$UTC[match(substr(call, 2, 4), asosstns$CALL)]
  ## use sprintf() to convert to character, "-0000" or "+0000"
  utcoffset <- sprintf("%+03d", utcoffset)
  TIME <- as.POSIXct(paste(TIME, utcoffset, "00", sep=""), 
                     format="%Y%m%d%H%M%z")
  ## return as data frame
  return(data.frame(CALL=call, TIME, TMPD, DPTP, PRS1, PRS2, PRS3, PCPN))
}