#' import otu table function
#'
#' This function reads your data file, formats sample ID's and taxa names, and converts
#' absolute abundances to relative abundances. The otu table must contain "otu" in the
#' file name with sample ID's as column names and taxa as row names.#'the metadata file
#' must have "meta" in the file name and contain matching sample ID's to the otu table in
#' the first column. To format date, there must be a column named "date" in the metadata file.
#' @param normalize argument to normalize data to relative abundance, defaults to TRUE
#' @param add_metadata argument to normalize data to relative abundance, defaults to TRUE
#' @param format_date argument to normalize data to relative abundance, defaults to TRUE
#' @keywords otu table
#' @export
#' @examples
#' import_otu_table(normalize=TRUE, add_metadata=TRUE, format_date=TRUE) #returns dataframe of normalized otu table with added metadata
#' import_otu_table(normalize=FALSE) #returns otu table with absolute abundances
import_otu_table <- function(normalize=TRUE, add_metadata=TRUE, format_date=TRUE){
  `%>%` <- magrittr::`%>%`
  #import rarefied family-level OTU table from Qiime
  files <- dir(pattern = "*.csv") # get file names
  raw.data <- read.csv(files[grep("otu", files)], header=TRUE, stringsAsFactors = FALSE, row.names = 1)

  #remove 'X' from sample ID's
  colnames(raw.data) = gsub("X", "", colnames(raw.data))

  #format taxa names
  rownames(raw.data) <- gsub("D_0__Archaea;Other;Other;Other;Other", "Archaea.Other", rownames(raw.data))
  rownames(raw.data) <- gsub("D_0__Bacteria;Other;Other;Other;Other", "Bacteria.Other", rownames(raw.data))
  rownames(raw.data) <- gsub("D_0__Bacteria;D_1__Proteobacteria;Other;Other;Other", "Proteobacteria.Other", rownames(raw.data))
  rownames(raw.data) <- gsub("D_0__Archaea;|D_0__Bacteria;|D_1__Proteobacteria;|D_1__|D_2__|D_3__|D_4__|D_5__", "", rownames(raw.data))
  rownames(raw.data) <- gsub("Unknown;Other;Other;Other;Other", "Unassigned",rownames(raw.data))
  rownames(raw.data) <- gsub(";", ".",rownames(raw.data))
  rownames(raw.data) <- gsub("Gammaproteobacteria.Betaproteobacteriales", "Betaproteobacteria.Betaproteobacteriales",rownames(raw.data))

  #delete rows that sum to 0
  raw.data = raw.data[ rowSums(raw.data[1:length(colnames(raw.data))])!=0, ]
  if(normalize==TRUE){
  #convert absolute abundances to relative abundances - note that colsums = 100 (not 1)
  data.rel.abundance <- raw.data %>%
    dplyr::mutate_at(dplyr::vars(1:length(colnames(raw.data))), dplyr::funs(as.numeric(paste0(100*./sum(.)))))

  rownames(data.rel.abundance) <- rownames(raw.data)

  data <- as.data.frame(t(data.rel.abundance))
  }else{
    data <- as.data.frame(t(raw.data))
  }
  if(add_metadata==TRUE){
    data$sample.name <- rownames(data)
    meta <- as.data.frame(read.csv(files[grep("meta", files)], header = TRUE, stringsAsFactors=FALSE), stringsAsFactors=FALSE)
    data <- merge(meta,data, by.x = c(colnames(meta)[1]),by.y=c('sample.name'),  all = TRUE)
  }else{
    data
  }
  if(format_date==TRUE){
    data$date <- paste0(lubridate::month(lubridate::mdy(data$date), label = TRUE),".", lubridate::year(lubridate::mdy(data$date)))
    data
  }else{
    data
  }
}





