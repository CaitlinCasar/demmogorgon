#' transform data function
#'
#' This function transforms your otu table without metadata using a variety of metrics. only one metric can be true at a time.
#' @param data your otu data without added metadata
#' @param  presence_absence a method to transform your data to binary, defaults to FALSE
#' @param  square_root a method to square root transform your data, defaults to FALSE
#' @param  one_over_x a method to transform your data by dividing 1 by each value, defaults to FALSE
#' @keywords transform
#' @export
#' @examples
#' data.pres.abs <- transform_data(data, presence_absence = TRUE)
transform_data <- function(data, presence_absence=FALSE, square_root=FALSE, one_over_x=FALSE){
  if(presence_absence==TRUE){
    data <-  as.data.frame(+ (data > 0))
  }else if(square_root==TRUE){
    data<- sqrt(data)
  }else if(one_over_x==TRUE){
    data <- 1/(data)
    data <- do.call(data.frame, lapply(data, function(x) {
      replace(x, is.infinite(x), 0)
    })
    )
  }else{
    print("Please choose a data transformation method.")
  }
}

