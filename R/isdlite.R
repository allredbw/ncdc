isdlite <- function(usaf, wban, begintime, endtime, overwrite.downlaod=F) {
  ## Retrieve and format NOAA ISD-Lite data for given station and time period.
  ## Arguments:
  ##  usaf: Air Force catalog station number
  ##  wban: NCDC WBAN number
  ##  begintime: character or POSIXct class defining start of time period
  ##  endtime: character or POSIXct class defining end of time period
  ##  overwrite.download: logical indicating overwrite of previous download
  ## Return: data frame
  
  ## check to see if begintime and endtime are of class character or POSIXct 
  ## set *.gmt appropriately
  if(is.character(begintime)==T && is.character(endtime)==T) {
    ## convert character timestamp to POSIXct, GMT timezone
    begintime.gmt <- as.POSIXct(begintime, tz="GMT")
    endtime.gmt <- as.POSIXct(endtime, tz="GMT")
  } else if(any(class(begintime)=="POSIXct") & any(class(endtime)=="POSIXct")) {
    ## convert POSIXct timestamp to timezone GMT for referenced
    begintime.gmt <- as.POSIXct(format(begintime, tz="GMT"), tz="GMT")
    endtime.gmt <- as.POSIXct(format(endtime, tz="GMT"), tz="GMT")
  } else {
    ## if not character or POSIXct, stop and give error message
    stop(paste("begintime and endtime must both be entered as",
               dQuote("YYYY-MM-DD HH:MM:SS"), "or a POSIXct class."))
  }
  
  ## get years from begintime and endtime
  dates <- seq(from=as.Date(begintime.gmt), 
               to=as.Date(endtime.gmt), by="year")
  dates.years <- as.integer(unique(format(dates, format="%Y")))
  
  ## create list to store 6405 files
  isdlite <- vector(mode="list", length=length(dates.years))
  
  ## retrieve files
  paths.isdlite <- lapply(dates.years, FUN=retrieveisdlite, usaf=usaf, 
                      wban=wban, force=overwrite.download)
  
  ## parse files
  isdlite <- lapply(paths.isdlite, FUN=parseisdlite)
  
  ##subset files according to begintime and endtime
  isdlite <- lapply(isdlite, FUN=function(x) {
    return(subset(x, x$TIME>=begintime.gmt & x$TIME<=endtime.gmt))})
  
  ## return files as data frame
  return(do.call(rbind, isdlite))
}