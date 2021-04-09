#' Compute the Ordinal Scope Scale for Movements
#'
#' This function read the output of the \code{ordgather} function
#' and compute the Ordinal scope scale for Indicator 4.5.1,4.5.2,4.5.3,4.5.4.
#'
#' @param x Output file from previous run - \code{ordgather} function
#'
#' @return Computed ordinal scale for Movements category
#'
#' @export
#' @examples
#' ordmove(x)
#'
ordmove <- function(x){
  #
  # Assign default of 0 for 4.5.1,4.5.2,4.5.3,4.5.4.
  #
  x$ord <- ifelse(x$who_code %in% c('4.5.1','4.5.3','4.5.2','4.5.4'), 0,x$ord)
  #
  # Assign 1 if who code is 4.5.2 and enforcement is recommended or who code is any of
  # 4.5.2,4.5.3,4.5.4 and enforcement is recommended.
  #
  x$ord <- ifelse(x$who_code == '4.5.2' & x$enforcement == 'recommended' | 
                                x$who_code %in% c('4.5.1','4.5.3','4.5.4') & x$enforcement == 'recommended', 
                              1,x$ord)
  #
  # Assign 2 if who code is any of 4.5.1,4.5.3,4.5.4 and enforcement is required or monitored
  #
  x$ord <- ifelse(x$who_code %in% c('4.5.1','4.5.3','4.5.4') & 
                                x$enforcement %in% c('required','monitored'), 
                              2,x$ord)
  #
  # Assign 3 if who code is any of 4.5.2 and enforcement is required or monitored and
  # target = partial curfew and not contain stay at home, weekend curfew or full curfew.
  #
  x$ord <- ifelse(x$who_code == '4.5.2' & 
                                x$enforcement %in% c('required','monitored') & 
                                tolower(x$targeted) == 'partial curfew' & 
                                !tolower(x$targeted) %in% c('stay at home','weekend curfew','full curfew'), 
                              3,x$ord)
  #
  # Assign 4 if who code is any of 4.5.2 and enforcement is required or monitored and
  # target is not partial curfew but contain stay at home or full curfew.
  #
  x$ord <- ifelse(x$who_code == '4.5.2' & 
                                x$enforcement %in% c('required','monitored') &
                                !grepl(paste(c('partial curfew'),collapse = "|"),tolower(x$targeted)) & 
                    grepl(paste(c('stay at home','full curfew'),collapse = "|"),tolower(x$targeted)), 
                              4,x$ord)
  #
  # Assign 5 if who code is any of 4.5.2 and enforcement is required or monitored and
  # target = weekend curfew or (partial curfew and stay at home) or target = full curfew or partial curfew.
  #
  x$ord <- ifelse(x$who_code == '4.5.2' & 
                                x$enforcement %in% c('required','monitored') &
                    (gsub(",.*$", "", x$targeted) == 'weekend curfew' | (gsub(",.*$", "", x$targeted) == 'partial curfew' & grepl(paste(c('stay at home'),collapse = "|"),tolower(x$targeted)))) |
                    (gsub(",.*$", "", x$targeted) == 'full curfew' | grepl(paste(c('partial curfew'),collapse = "|"),tolower(x$targeted))), 5 ,x$ord)

  #
  # if measure stage is new, extension or modification, the score does not go down
  x$ord <- ifelse(x$who_code %in% c('4.5.1','4.5.3','4.5.2','4.5.4') & 
                    x$measure_stage %in% c('new','modification','extension') &
                    x$ord < lag(x$ord),lag(x$ord),x$ord)
  #
  x <- replace_na(x, list(ord = 1))
  #
  #
  return(x)
}
