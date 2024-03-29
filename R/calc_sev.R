#' Compute the PHSM Severity Index
#'
#' This function takes the binary and ordinal scores
#' and compute the PHSM severity index using the WHO EURO formula
#'
#' @param x CSV file with binary and ordinal scales scores
#'
#' @return Computed PHSM severity index
#'
#' @export
#' @examples
#' calc_sev(x)
#'
calc_sev <- function(x){
  #
  # Rename categories
  # 
  x <- x %>%
    select("who_code","measure_stage",  "country_territory_area","date_start","bin","ord","E_max","Q_max") %>%
    mutate(
      category = case_when(who_code %in% c('4.1.1','4.1.2') ~ 'School',
                           who_code == '1.4' ~ 'Mask',
                           who_code %in% c('4.2.1','4.2.2') ~ "Business",
                           who_code %in% c('4.3.1','4.3.2','4.3.3','4.3.4') ~ "Gatherings",
                           who_code %in% c('4.5.1','4.5.2','4.5.3', '4.5.4') ~ "Movements",
                           who_code %in% c('5.2','5.3','5.5','5.7','5.8','5.9') ~ "Travel"))
  #
  # Get maximum binary and ordinal scores per day
  #
  x <- x %>% group_by(date_start,who_code,country_territory_area,category,measure_stage,E_max,Q_max) %>%
    summarise(
      binary = max(bin, na.rm = TRUE), 
      ordinal = max(ord, na.rm = TRUE))
  #
  # Calculate SI index (formula used is from the EURO methodology paper)
  #
  # Mask
  #
  x$SI <- ifelse(x$category=='Mask', round(100*((as.numeric(x$ordinal)-0.6*(1-as.numeric(x$binary)))/3)),"")
  #
  # School
  #                              
  x$SI <- ifelse(x$category == 'School', round(100*((as.numeric(x$ordinal)-0.8*(1-as.numeric(x$binary)))/4)),x$SI)
  #
  # This is still for School. From Chris, where the SI is 5, change to 25. (should this be applied to all others???)
  #
  x$SI <- ifelse(x$category == 'School' &
                   x$SI == 5,25,x$SI)
  #                 
  # Business
  #                             
  x$SI <- ifelse(x$category == 'Business', round(100*((as.numeric(x$ordinal)-0.6*(1-as.numeric(x$binary)))/3)),x$SI)
  #                             
  # Gathering
  #                               
  x$SI <- ifelse(x$category == 'Gatherings', round(100*((as.numeric(x$ordinal)-0.8*(1-as.numeric(x$binary)))/4)),x$SI) 
  #
  # Movement
  #                                
  x$SI <- ifelse(x$category == 'Movements', round(100*((as.numeric(x$ordinal)-1*(1-as.numeric(x$binary)))/5)),x$SI)
  #
  # Travel
  #
  #x$SI <- ifelse(x$category == 'Travel',
  #                  ifelse(as.numeric(x$binary) == 4,round(100*(6/6)), round(100*((as.numeric(x$binary) + as.numeric(x$ordinal))/6))),x$SI)
  #
  # Lines 64 and 65 are the original formula for Travel, but modified below with Orlagh
  #
  x$SI <- ifelse(x$category == 'Travel', round(100*(as.numeric(x$ordinal)/6)),x$SI)
  
  # where ordinal and binary scores are 0, it gives a negative SI, so need to remove all such cases.
  #
  x <- x %>%
    filter(!(binary == '0' & ordinal == '0'))
  
  return(x)
}
#ab <- calc_sev(aa)
#ab <- calc_sev(data7)
