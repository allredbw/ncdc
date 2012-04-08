retrieve6405 <- function(call, yearmonth, force) {
  ## Downlaod 6405 file from NCDC to temporary file
  ## Arguments:
  ##  call: four letter call sign
  ##  yearmonth: character string indicating year, month; e.g. Jan 2012: 201201
  ## Returns: path to tempory file
  
  ## extract year from yearmonth
  year <- substr(x=yearmonth, start=1, stop=4)
  month <- substr(x=yearmonth, start=5, stop=6)
  file6405 <- paste("64050", call, yearmonth, ".dat", sep="")
  
  ## check to see if file exists in tempdir()
  ## if not, download it
  if(length(Sys.glob(file.path(tempdir(),file6405)))==1 & force==F) {
    return(file.path(tempdir(),file6405))
  } else {
    ## set path for 6405 file on NCDC ftp server
    NCDCpath <- paste("ftp://anonymous:email", 
                      "@ftp.ncdc.noaa.gov/pub/data/asos-onemin/6405-",
                      year, "/64050", call, yearmonth, ".dat", 
                      sep="")
    ## set local path to save 6405 file in temp directory
    localpath <- paste(tempdir(), "/", basename(NCDCpath), sep="")
    ## verify 6405 exists and is retrievable
    if(verify6405(path=NCDCpath)==F) {
      return(NA)
    }
    download.file(url=NCDCpath, destfile=localpath, quiet=T)
    return(file.path(tempdir(),file6405))
  }
}