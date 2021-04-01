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
                              !grepl('public spaces', tolower(x$targeted)), 2,x$ord)
  #
  # Assign 3 if enforcement is required or monitored and target
  # is public spaces or indoor spaces or (indoor spaces & outdoor spaces)
  #
  x$ord  <- ifelse(x$who_code=='1.4' & 
                     x$enforcement %in% c('required','monitored') & 
                     !x$enforcement == 'recommended' &
                               grepl('public spaces', tolower(x$targeted)) |
                              grepl('indoor spaces', tolower(x$targeted)) |
                               (grepl('indoor spaces', tolower(x$targeted)) &
                                  grepl('outdoor spaces', tolower(x$targeted))), 3,x$ord) 
  #
  ########################################################
  # Scores are assigned based on manual check of scores 
  # from above and adjust the score to agree with that.
  #########################################################
  #
  # If enforcement is 'not applicable' or recommended and
  # ordinal scale is already 2 or 3, change ordinal scale to 1.
  #
  x$ord <- ifelse(x$enforcement %in% c("not applicable","recommended") &
                    x$ord %in% c(2,3), 1, x$ord)
  #
  # if measure stage is neither phase-out or finish, and
  # previous measure stage is same as current measure stage, and
  # next measure stage is same as current measure stage, then,
  # if precious ordinal score is 3, make current score also 3.
  #
  x$ord <- ifelse(!x$measure_stage %in% c("phase-out", "finish") & 
                     x$measure_stage == lag(x$measure_stage, n=1L) &
                     x$measure_stage == lead(x$measure_stage, n=1L) &
                     lag(x$ord) == 3 &
                     x$enforcement %in% c("required","monitored"),3, x$ord)
  #
  return(x)
}
#data2<-ordmasks(data1)

