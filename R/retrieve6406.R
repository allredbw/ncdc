retrieve6406 <- function(call, yearmonth) {
  ## Downlaod 6406 file from NCDC to temporary file
  ## Arguments:
  ##  call: four letter call sign
  ##  yearmonth: character string indicating year, month; e.g. Jan 2012: 201201
  ## Returns: path to tempory file
  
  ## extract year from yearmonth
  year <- substr(x=yearmonth, start=1, stop=4)
  file6406 <- paste("64060", call, yearmonth, ".dat", sep="")
  
  ## check to see if file exists in tempdir()
  ## if not, download it
  if(length(Sys.glob(file.path(tempdir(),file6406)))==1) {
    return(file.path(tempdir(),file6406))
  } else {
    NCDCpath <- paste("ftp://anonymous:email", 
                      "@ftp.ncdc.noaa.gov/pub/data/asos-onemin/6406-",
                      year, "/64060", call, yearmonth, ".dat", 
                      sep="")
    localpath <- paste(tempdir(), "/", basename(NCDCpath), sep="")
    download.file(url=NCDCpath, destfile=localpath, quiet=T)
    return(file.path(tempdir(),file6406))
  }
}