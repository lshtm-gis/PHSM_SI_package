#' Compute the Ordinal Scope Scale for Gathering
#'
#' This function read the output of the \code{ordbusiness} function
#' and compute the Ordinal scope scale for Indicator 4.3.1,4.3.2,4.3.3,4.3.4.
#'
#' @param x Output file from previous run - \code{ordbusiness} function
#'
#' @return Computed ordinal scale for Gathering category
#'
#' @export
#' @examples
#' ordgather(x)
#'
ordgather <- function(x){
  #
  # First clean up the target variable of '<' and '>'
  #
  pattern <- c("<")
  #
  replacement <- ""
  #
  x$targeted <- stringr::str_replace_all(x$targeted, pattern,replacement)
  #
  pattern <- c(">")
  #
  replacement<- ""
  #
  x$targeted <- stringr::str_replace_all(x$targeted, pattern,replacement)
  #
  # Assign 0 as default for all 4.3.1,4.3.2,4.3.3,4.3.4
  #
  x$ord <- ifelse(x$who_code %in% c('4.3.1','4.3.2','4.3.3','4.3.4'), 0,x$ord)
  #
  # Assign 1 if target does not contain gatherings, or all gatherings or any number less than 10
  #
  x$ord <- ifelse(x$who_code %in% c('4.3.1','4.3.2','4.3.3','4.3.4') & 
                    !tolower(x$targeted) %in% c('all','all gatherings','gatherings','>10','<10',10,'10',1,2,3,4,5,6,7,8,9,'[0-9]','all events'), 1,x$ord)
  x$ord <- ifelse(x$who_code %in% c('4.3.1','4.3.2'),1,x$ord)
  #
  # Assign 2 for opposite of above case
  #
  # This is for cases where there is restriction to gatherings
  x$ord <- ifelse(x$who_code %in% c('4.3.3','4.3.4') & 
                      tolower(strsplit(x$targeted,split=",")) %in% c('all','all gatherings','gatherings',10,1,2,3,4,5,6,7,8,9,0-9,'all events','public gatherings'), 2,x$ord)
  #
  # Because grep matches all related terms, i.e. given 'gatherings' as keyword, it matches 'gatherings', 'all gatherings', 'large public gatherings', 'public gatherings'. 
  # And following Orlagh's manual checks, 'large public gatherings' is different, and < severe than public gatherings, so return large public gatherings back to 1.
  #
  x$ord <- ifelse(x$who_code %in% c('4.3.1','4.3.2','4.3.3','4.3.4') & 
                                   grepl(paste(c('large'),collapse = "|"),
                                         tolower(x$targeted)) ,1,x$ord)
  #
  x$ord <- ifelse(x$who_code %in% c('4.3.3','4.3.4') &
                   gsub(",.*$", "", x$targeted) == 10,2,x$ord)
  #
  x$ord <- ifelse(x$who_code %in% c('4.3.3','4.3.4') &
                    gsub(",.*$", "", x$targeted) < 10 &
                    gsub(",.*$", "", x$targeted) > 1,2,x$ord)
                    
  #
  x <- replace_na(x, list(ord = 1))
  #
  
  return(x)
}
