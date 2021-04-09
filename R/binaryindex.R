#' Compute the Binary Scope Scale
#'
#' This function read the output of the \code{dataprep} function
#' and compute the Binary scope scale.
#'
#' @param x Output file from previous run - \code{dataprep} function
#'
#' @return In addition to the input columns, a `bin` variable
#'
#' @export
#' @examples
#' binaryindex(x)
#'
binaryindex <- function(x){
  #
  # Assign '0' if admin_level is not national or targeted = zones, else assign '1'.
  #
  x$bin <- ifelse(!x$admin_level == 'national' | 
                     grepl('zones', tolower(x$targeted)), 
                   0,'1')
  #
  return(x)
}
#data1<- binaryindex(data_f)
