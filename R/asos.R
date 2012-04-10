asos <- function(call, begintime, endtime, file6405=T, file6406=T,
                 overwrite.download=F, mcores=F) {
  ## Retrieve and format NOAA ASOS data for given station and time period.
  ## Arguments:
  ##  call: four letter station call sign
  ##  begintime: character or POSIXct class defining start of time period
  ##  endtime: character or POSIXct class defining end of time period
  ##  file6405: logical indicating to get 6405 files; wind data
  ##  file6406: logical indicating to get 6406 files; temp, pressure, precip
  ## Return: data frame
  
  if(file6405==F && file6406==F) {
    stop.msg <- paste("file6405==F and files6406==F; no files to download.")
    stop(stop.msg)
  }
  ## convert call to uppercase
  call <- toupper(call)
  
  ## determine UTC offset for station
  utcoffset <- asosstns$UTC[match(substr(call, 2, 4), asosstns$CALL)]
  ## use sprintf() to convert to character, "-0000" or "+0000"
  utcoffset.char <- sprintf("%+03d", utcoffset)
  
  ## check to see if begintime and endtime are of class character or POSIXct 
  ## set *.local (local to station) and *.gmt appropriately
  if(is.character(begintime)==T && is.character(endtime)==T) {
    ## convert character timestamp to POSIXct
    begintime.local <- as.POSIXct(paste(begintime, utcoffset.char, "00",
                                        sep=""),
                                  format="%Y-%m-%d %H:%M:%S%z")
    endtime.local <- as.POSIXct(paste(endtime, utcoffset.char, "00", sep=""),
                                format="%Y-%m-%d %H:%M:%S %z")
    ## Convert to timezone GMT for reference
    begintime.gmt <- as.POSIXct(format(begintime.local, tz="GMT"), tz="GMT")
    endtime.gmt <- as.POSIXct(format(endtime.local, tz="GMT"), tz="GMT")
  } else if(any(class(begintime)=="POSIXct") & any(class(endtime)=="POSIXct")) {
    ## convert POSIXct timestamp to timezone GMT for referenced
    begintime.gmt <- as.POSIXct(format(begintime, tz="GMT"), tz="GMT")
    endtime.gmt <- as.POSIXct(format(endtime, tz="GMT"), tz="GMT")
    ## begintime local
    begintime.local.char <- format(as.POSIXct(begintime.gmt) + utcoffset*60*60)
    begintime.local <- as.POSIXct(paste(begintime.local.char, utcoffset.char, 
                                        "00", sep=""),
                                  format="%Y-%m-%d %H:%M:%S%z")
    ## endtime local
    end.local.char <- format(as.POSIXct(endtime.gmt) + utcoffset*60*60)
    endtime.local <- as.POSIXct(paste(endtime.local.char, utcoffset.char, 
                                        "00", sep=""),
                                  format="%Y-%m-%d %H:%M:%S%z")
  } else {
    ## if not character or POSIXct, stop and give error message
    stop(paste("begintime and endtime must both be entered as",
               dQuote("YYYY-MM-DD HH:MM:SS"), "or a POSIXct class."))
  }
  
  dates <- seq(from=as.Date(begintime.gmt+utcoffset*60*60), 
               to=as.Date(endtime.gmt+utcoffset*60*60), by="day")
  dates.yearmonth <- unique(format(dates, format="%Y%m"))
  
  ## 6405 files
  if(file6405==T) {
  
    ## create list to store 6405 files
    all6405 <- vector(mode="list", length=length(dates.yearmonth))
    
    ## retrieve files
    lapply(dates.yearmonth, FUN=retrieve6405, call=call,
           force=overwrite.download)
    
    ## multicore options
    if(mcores==T | is.numeric(mcores)==T) {
      if(mcores==T) ncores <- detectCores()-1
      if(is.numeric(mcores)==T) ncores=round(mcores)
      if(.Platform$OS.type=="unix") {
        all6405 <- mclapply(dates.yearmonth, FUN=parse6405, call=call,
                            force=F, mc.cores=ncores)
      } else if(.Platform$OS.type=="windows") {
        c1 <- makeCluster(getOption("cl.cores", ncores))
        all6405 <- parLapply(c1, dates.yearmonth, fun=parse6405, call=call,
                             force=F)
        stopCluster(c1)
      } } else { all6405 <- lapply(dates.yearmonth, FUN=parse6405, call=call, 
                                 force=F)
    }
    
    ##  Subset 6405 data according to begin and end time
    all6405[[1]] <- subset(all6405[[1]], TIME>=begintime.local 
                           & TIME<=endtime.local)
    all6405[[length(all6405)]] <- subset(all6405[[length(all6405)]],
                                         TIME>=begintime.local 
                                         & TIME<=endtime.local)
  }
  
  ## 6406 files
  if(file6406==T) {
    
    ## create list to store 6406 files
    all6406 <- vector(mode="list", length=length(dates.yearmonth))
    
    ## retrieve files
    lapply(dates.yearmonth, FUN=retrieve6406, call=call,
           force=overwrite.download)
    ## mutlicore options
    if(mcores==T | is.numeric(mcores)==T) {
      if(mcores==T) ncores <- detectCores()-1
      if(is.numeric(mcores)==T) ncores=round(mcores)
      if(.Platform$OS.type=="unix") {
        all6406 <- mclapply(dates.yearmonth, FUN=parse6406, call=call,
                            force=F, mc.cores=ncores)
      } else if(.Platform$OS.type=="windows") {
        c1 <- makeCluster(getOption("cl.cores", ncores))
        all6406 <- parLapply(c1, dates.yearmonth, fun=parse6406, call=call,
                             force=F)
        stopCluster(c1)
      } } else { all6406 <- lapply(dates.yearmonth, FUN=parse6406, call=call, 
                                 force=F)
    }
    
    ##  Subset 6406 data according to begin and end time
    all6406[[1]] <- subset(all6406[[1]], TIME>=begintime.local 
                           & TIME<=endtime.local)
    all6406[[length(all6406)]] <- subset(all6406[[length(all6406)]],
                                         TIME>=begintime.local 
                                         & TIME<=endtime.local)
  }
  
  ## return appropriate files as data frames
  if(file6405==T && file6406==T) {
    data6405 <- do.call(rbind, all6405)
    data6406 <- do.call(rbind, all6406)
    return(merge(data6406, data6405, all=T))
  } else if(file6405==T && file6406==F) {
    return(do.call(rbind, all6405))
  } else if(file6405==F && file6406==T) {
    return(do.call(rbind, all6406))
  }
}