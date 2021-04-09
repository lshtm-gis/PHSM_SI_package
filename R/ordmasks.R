#' Compute the Ordinal Scope Scale for Masks
#'
#' This function read the output of the \code{binaryindex} function
#' and compute the Ordinal scope scale for Indicator 1.4.
#'
#' @param x Output file from previous run - \code{binaryindex} function
#'
#' @return In addition to the input columns, a `ord` variable
#'
#' @export
#' @examples
#' ordmasks(x)
#'
ordmasks <- function(x){
  #
  # Assign 0 as default for all 1.4
  #
  x$ord <- ifelse(x$who_code=='1.4', 0,'')
  #
  # Assign 1 if enforcement is recommended 
  #
  x$ord <- ifelse(x$who_code=='1.4' & 
                    x$enforcement == 'recommended', 1,x$ord)
  #
  # Assign 2 if enforcement is required or monitored and
  # target is not public spaces
  #
  x$ord <- ifelse(x$who_code=='1.4' & x$enforcement %in% c('required','monitored') & 
                    !grepl('public spaces', tolower(x$targeted)) |
                    grepl('transport', tolower(x$targeted)) |
                    grepl('businesses', tolower(x$targeted)), 2,x$ord)
  #
  # Assign 3 if enforcement is required or monitored and target
  # is public spaces or indoor spaces or (indoor spaces & outdoor spaces)
  #
  x$ord  <- ifelse(x$who_code=='1.4' & 
                     x$enforcement %in% c('required','monitored') & 
                     !x$enforcement == 'recommended' &
                               grepl('public spaces', tolower(x$targeted)) |
                               grepl('indoor spaces', tolower(x$targeted)) |
                     grepl('outdoor spaces', tolower(x$targeted)) |
                               (grepl('indoor spaces', tolower(x$targeted)) &
                                  grepl('outdoor spaces', tolower(x$targeted))), 3,x$ord)
#
# Phase-out or finish can not go up
#
  x$ord <- ifelse(x$who_code=='1.4'& 
                    x$measure_stage %in% c("phase-out","finish") &
                    x$ord > lag(x$ord),lag(x$ord),x$ord)
#  
  x <- replace_na(x, list(ord = 1))
#
  return(x)
}
#ad<-ordmasks(ac)

