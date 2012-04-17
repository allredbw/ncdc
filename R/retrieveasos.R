retrieveasos <- function(call, yearmonth, file, force) {
  ## Downlaod 6405 file from NCDC to temporary file
  ## Arguments:
  ##  call: four letter call sign
  ##  yearmonth: character string indicating year, month; e.g. Jan 2012: 201201
  ##  file: integer indicating 6405 or 6406 file
  ##  force: logical indicating to force download
  ## Returns: path to tempory file
  
  ## extract year from yearmonth
  year <- substr(x=yearmonth, start=1, stop=4)
  month <- substr(x=yearmonth, start=5, stop=6)
  asosfile <- paste(file, "0", call, yearmonth, ".dat", sep="")
  
  ## check to see if file exists in tempdir()
  ## if not, download it
  if(length(Sys.glob(file.path(tempdir(), asosfile)))==1 & force==F) {
    return(file.path(tempdir(), asosfile))
  } else {
    ## set path for file on NCDC ftp server
    NCDCpath <- paste("ftp://anonymous:email", 
                      "@ftp.ncdc.noaa.gov/pub/data/asos-onemin/", file, "-",
                      year, "/", file, "0", call, yearmonth, ".dat", 
                      sep="")
    ## set local path to save file in temp directory
    localpath <- paste(tempdir(), "/", basename(NCDCpath), sep="")
    ## verify file exists and is retrievable
    if(verify(path=NCDCpath)==F) {
      return(NA)
    }
    download.file(url=NCDCpath, destfile=localpath, quiet=T)
    return(file.path(tempdir(),asosfile))
  }
}