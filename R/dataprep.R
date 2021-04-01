#' Prepare Data Set for the Severity Calculation
#'
#' This function read a csv data (PHSM mistress or master files)
#' and return only the variables needed for the calculation
#' of the severity index (SI).
#'
#' @param x csv file - mistress or master file
#' @param country=NULL Filter for a specific country, else all.
#'
#' @return 8 variables required for the calculation of the SI:
#'
#' @export
#' @examples
#' dataprep(x)
#' dataprep(x,'Algeria')
#'
dataprep <- function(x, country=NULL){
  #
  # Read the csv file
  # 
  data <- read.csv(x,encoding = "latin1")
  #
  # If second argument is null,
  # Get all country records
  # Else filter for specific country
  #
  if(is.null(country)){
    data = data
  
  } else {
    data = data %>% 
      filter(country_territory_area == country)
  }
  #
  # Select the variables required
  #
  data <- data %>%
    filter(!targeted %in% c('news','transport'))
  
  data <- data %>%
    select(who_id, who_code, admin_level, date_start, measure_stage,targeted,enforcement,country_territory_area)
  #
  # Sort the data variable
  #
  data = data[order(as.Date(data$date_start, format="%Y-%m-%d")),]
  #
  return(data)
}
#data_ff <- dataprep('/home/sewedo/Documents/lshtm/severity_index/1803/mistress_202103161.csv','France')
#data_f <- dataprep('mistress_202103161.csv','Algeria')
