#' create metadata function
#'
#' This function imports your otu table metadata. You must have a csv file with "meta" in the file name in the current working directory.
#' @param  date argument to format dates in your metadata, your csv file must have a column named "date", defaults to TRUE
#' @keywords metadata
#' @export
#' @examples
#' metadata <- create_metadata()
create_metadata <- function(date=TRUE){
  files <- dir(pattern = "*.csv") # get file names
  meta <- as.data.frame(read.csv(files[grep("meta", files)], header = TRUE, stringsAsFactors=FALSE), stringsAsFactors=FALSE)
  if(date==TRUE){
  meta$date <- paste0(lubridate::month(lubridate::mdy(meta$date), label = TRUE),".", lubridate::year(lubridate::mdy(meta$date)))
  meta
  }
}
