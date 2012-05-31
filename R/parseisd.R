parseisd <- function(localpath) {
  ## Parse and format ISD Lite file
  ## Arguments:
  ##  localpath: path to ISD Lite file; returned by retrievedisd()
  ## Return: data frame of ISD Lite information
  
  ## read in ISD Lite file
  fileisd <- read.table(localpath, as.is=T, header=F, na.strings="-9999",
                        colClasses=c("character", "character", "character", 
                                     "character", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric"),
                        col.names=c("Year", "Month", "Day", "Hour", "TMPD", 
                                    "DPTP", "PRES", "D360", "M2SP", "CLCO",
                                    "PC1H", "PC6H"))
  
  ## Add timestamp to ISD Lite file
  fileisd$TIME <- as.POSIXct(paste(fileisd$Year, fileisd$Month, fileisd$Day,
                                   fileisd$Hour, "00", sep=""), 
                             format="%Y%m%d%H%M", tz="GMT")
  
  ## scale variables appropriately
  fileisd$TMPD <- fileisd$TMPD / 10
  fileisd$DPTP <- fileisd$DPTP / 10
  fileisd$M2SP <- fileisd$M2SP / 10
}