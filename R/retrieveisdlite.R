retrieveisdlite <- function(usaf, wban, year, force) {
  ## Retreive ISD lite file for a given station, year
  ## Arguments:
  ##  usaf: Air Force catalog station number
  ##  wban: NCDC WBAN number
  ##  year: integer, year to retrieve
  ##  force: logical indicating to force download
  ## Returns: path to temporary file
  
  isdfile <- paste(usaf, "-", wban, "-", year, ".gz", sep="")
  
  ## check to see if file exists in tempdir()
  ## if not, download it
  if(length(Sys.glob(file.path(tempdir(), isdfile)))==1 & force==F) {
    return(file.path(tempdir(), isdfile))
  } else {
    ## set path for file on NCDC ftp server
    NCDCpath <- paste("ftp://anonymous:email", 
                      "@ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/", year, "/",
                      isdfile, sep="")
    ## set local path to save file in temp directory
    localpath <- paste(tempdir(), "/", basename(NCDCpath), sep="")
    ## verify file exists and is retrievable
    if(verify(path=NCDCpath)==F) {
          return(NA)
    }
    download.file(url=NCDCpath, destfile=localpath, quiet=T)
    return(file.path(tempdir(),isdfile))
  }
}