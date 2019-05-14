#' create color dictionary function
#'
#' This function generates a color dictionary from a csv file with taxa names in column 1, and six digit numbers for colors in column 2
#' @param data your otu data without added metadata
#' @param metadata your metadata with sample name in the first column and a column named 'site'
#' @keywords color
#' @export
#' @examples
#' create_color_dict(data, metadata)

create_color_dict <- function(data, metadata){
  files <- dir(pattern = "*.csv") # get file names
  color_dict_data <- read.csv(files[grep("color", files)], header = TRUE)
  color_dict_data <- as.data.frame(lapply(color_dict_data, function(x) as.character(gsub(pattern = "^0$", "000000", x))))

  #generate phylum color key
  color_dict <-c(rep('#000000', length(unique(unlist(metadata[1])))), rep('#000000', length(unique(metadata$site))), paste0('#', unlist(color_dict_data[2])))
  names(color_dict) <- c(as.character(unique(unlist(metadata[1]))), as.character(unique(metadata$site)),as.character(unlist(color_dict_data[1])))
  
  #double check that all phyla have color assignments in the dataset
  data_phyla <- unique(cbind(phylum = gsub( "[.].*$", "", colnames(data))))
  #print out names of any phyla missing int the color key
  missing_data_phyla <- data_phyla[(!data_phyla %in% names(color_dict))]
  
  if(length(missing_data_phyla > 0)) {
    print(paste0("Please create colors for the following taxa: ", missing_data_phyla))
  }else{
    #if this throws an error, a hex code is invalid and needs to be corrected
    print("validating your hex colors...")
    color_test <- col2rgb(color_dict)
  }
  if(length(color_test > 0 )){
    print("your colors are valid!")
    color_dict
  }

  
}
  