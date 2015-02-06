batch.read <- function(file, ...) {

  x <- read.csv(file=file, stringsAsFactors=FALSE, ...)
  
  return(x)
}
