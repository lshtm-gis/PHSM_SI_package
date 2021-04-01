#' Call the function that generate the final data
#'
#' 
#' 
#' @examples
#' forplot(x)
#' forplot(x,country)
forplot <- function(x,country=NULL){
  aa2 <- sevindex(x)
  if(is.null(country)){
    data <- data.frame()
    for (country in unique(aa2$country_territory_area)){
      data_ <- prep_plot(aa2,country)
      data <- bind_rows(data, data_)
    }
  } else {
    data <- prep_plot(aa2,country)
  }
  return(data)
}