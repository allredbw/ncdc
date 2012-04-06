.verifyasos<- function(path) {
  ## Verify that asos station data can be downloaded
  ##
  ## Arguments: none
  ## Returns: logical indicating success
  url <- "http://www.ncdc.noaa.gov/homr/file/asos-stations.txt"
  internetoption <- getOption("internet.info")
  options(show.error.messages=FALSE, internet.info=3)
  checkconn <- try(readLines(url, n=2))
  options(show.error.messages=TRUE, internet.info=internetoption)
  if (inherits(checkconn,"try-error")) return(FALSE)
  else return(TRUE)
}

.downloadasos <- function() {
  ## Retrieve asos station information
  ##
  ## Arguments: none
  ## Returns: data frame with station information
  
  url <- "http://www.ncdc.noaa.gov/homr/file/asos-stations.txt"
  localpath <- paste(tempdir(), "/", basename(url), sep="")
  
  if(.verifyasos()==FALSE) {
    warning(paste("Check", url, "for connectivity\n",
                  "and run updateasos() to update station list.", sep=" "))
  } else {
    ## turn of warning message for end of line warning
    warnoption <- getOption("warn")
    options(warn=-1)
    
    ## download file
    download.file(url=url, destfile=localpath, quiet=T)
    
    ## read in file
    asosstns <- read.fwf(localpath, as.is=T, comment.char="", skip=2,
                         strip.white=T,
                         width=c(8, -1, 5, -1, 6, -1, 4, -1, 30, -1, 30, -1, 20, 
                                 -1, 2, -1, 30, -1, 9, -1, 10, -1, 6, -1, 3, -1, 
                                 50),
                         colClasses=c("numeric", "numeric", "numeric", 
                                      "character", "character", "character", 
                                      "character", "character", "character", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "character"),
                         col.names=c("NCDCID", "WBAN", "COOPID", "CALL", "NAME",
                                     "ALT_NAME", "COUNTRY", "ST", "COUNTY", 
                                     "LAT", "LON", "ELEV", "UTC", "STNTYPE"))
    options(warn=warnoption)
    return(asosstns)
  }
}

asosstns <- .downloadasos()