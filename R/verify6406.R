verify6406<- function(path) {
  ## Verify that the requested 6406 exists
  ##
  ## Arguments:
  ##  path: path to 6406 on NCDC ftp site
  ## Returns: logical indicating success
  internetoption <- getOption("internet.info")
  options(show.error.messages=FALSE, internet.info=3)
  checkconn <- try(readLines(path, n=1))
  options(show.error.messages=TRUE, internet.info=internetoption)
  if (inherits(checkconn,"try-error")) return(FALSE)
  else return(TRUE)
}