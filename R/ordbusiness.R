#' Compute the Ordinal Scope Scale for Businesses
#'
#' This function read the output of the \code{ordschools} function
#' and compute the Ordinal scope scale for Indicator 4.2.1 and 4.2.2.
#'
#' @param x Output file from previous run - \code{ordschools} function
#'
#' @return Computed ordinal scale for Businesses category
#'
#' @export
#' @examples
#' ordbusiness(x)
#'
ordbusiness <- function(x){
  #
  #
  # Define a list of keywords in Businesses which are of interest
  #
  all_target_b <- c('hospitality','entertainment','fitness','recreation','industry','retail','offices',
                    'beauty','tourism','religion','health','venues','non-essential businesses')
  #
  # A second list excluding 'non-essential businesses'
  #
  all_target_c <- c('hospitality','entertainment','fitness','recreation','industry','retail','offices',
                    'beauty','tourism','religion','health','venues')
  #
  # Identify Businesses that are shut - i.e. if who_code = 4.2.2, and the record's 
  # target is in the list of keywords above, then the business is closed.
  # e.g. if who_code = 4.2.2, target = retail, then it means
  # retail is closed
  #
  x <- x %>% mutate(
    bus_shut = ifelse(who_code == '4.2.2', apply(x, 1, function(x) {
      paste(all_target_b[all_target_b %in% strsplit(x[6], ",", fixed=TRUE)[[1]]], collapse=",")
    }), ''))
  #
  # Identify Businesses that are opened - i.e. if who_code = 4.2.1, and the record's 
  # target is in the list of keywords above, then the business is opened.
  # e.g. if who_code = 4.2.1, target = retail, then it means
  # retail is opened.
  #
  x <- x %>% mutate(
    bus_open = ifelse(who_code == '4.2.1', apply(x, 1, function(x) {
      paste(all_target_b[all_target_b %in% strsplit(x[6], ",", fixed=TRUE)[[1]]], collapse=",")
    }), ''))
  #
  # For Business closing (4.2.2), identify businesses that remain open.
  # e.g. if who_code = 4.2.2 and target = 'hospitality', then it means
  # 'hospitality' is closed while others are opened. So basically get 
  # difference between what is reported closed and list of keyword
  #
  x <- x %>% mutate(
    shut_left_opened = ifelse(who_code == '4.2.2', apply(x, 1, function(x) {
      paste(all_target_c[!all_target_c %in% strsplit(x[8], ",", fixed=TRUE)[[1]]], collapse=",")
    }), ''))
  #
  # For Business adapting (4.2.1), identify businesses that remain closed.
  # e.g. if who_code = 4.2.1 and target = 'hospitality', then it means
  # 'hospitality' is opened while others are closed. So basically get 
  # difference between what is reported opened and list of keyword
  #
  x <- x %>% mutate(
    opened_left_shut = ifelse(who_code == '4.2.1', apply(x, 1, function(x) {
      paste(all_target_c[!all_target_c %in% strsplit(x[9], ",", fixed=TRUE)[[1]]], collapse=",")
    }), ''))
  #
  # End of data preparation
  #
  # Now to assign the ordinal code
  #
  # Assign 0 as default for all 4.2.1 and 4.2.2
  #
  x$ord <- ifelse(x$who_code %in% c('4.2.1','4.2.2'), 0,x$ord)
  #
  # Assign 3 if who_code = 4.2.2 and all businesses or all essential business or 
  # all list of items in business keyword is closed.
  #
  x$ord <- ifelse(x$who_code == '4.2.2' & 
                                 (x$bus_shut %in% c('all businesses', 'non-essential businesses') |
                                 list.count(unique(x$bus_shut)) == list.count(all_target_c)), 3, x$ord)
  #
  # if who_code = 4.2.1 and all businesses or all essential business or all list of items in business keyword is opened, ordinal scale = 1
  # 
  x$ord <- ifelse(x$who_code == '4.2.1' & 
                                 (x$bus_open %in% c('all businesses', 'non-essential businesses') |
                                 list.count(unique(x$bus_open)) == list.count(all_target_c)), 1, x$ord)
  #
  # Assign 2 if who_code = 4.2.2 and not all businesses or all non essential business 
  # are closed.
  #
  x$ord <- ifelse(x$who_code == '4.2.2' & 
                                 (!x$bus_shut %in% c('all businesses', 'non-essential businesses')), 2, x$ord)
  #
  # Assign 2 if who_code = 4.2.1 and not all businesses or all non essential business 
  # are opened.
  #
  x$ord <- ifelse(x$who_code == '4.2.1' & 
                                 (!x$bus_open %in% c('all businesses', 'non-essential businesses')), 2, x$ord)
  #
  # To adjust for fluctuations in coding, if current measure stage is phase out, and
  # previous measure stage (up to 3 lags) is phase out, and previous target is non-essential businesses
  # then current ordinal scale is changed to 1.
  #
  x$ord <- ifelse(x$who_code %in% c('4.2.1','4.2.2') & 
                    x$measure_stage == 'phase-out' &
                    (lag(x$measure_stage, n=1L) == 'phase-out' |
                    lag(x$measure_stage, n=2L) == 'phase-out' |
                    lag(x$measure_stage, n=3L) == 'phase-out') &
                    (grepl('non-essential businesses', lag(tolower(x$targeted),n=1L)) |
                    grepl('non-essential businesses', lag(tolower(x$targeted),n=2L)) |
                  grepl('non-essential businesses', lag(tolower(x$targeted),n=3L)) |
                    grepl('non-essential businesses', lag(tolower(x$targeted),n=4L)) |
                    grepl('non-essential businesses', lag(tolower(x$targeted),n=5L)) |
                    grepl('non-essential businesses', lag(tolower(x$targeted),n=6L)) |
                    grepl('non-essential businesses', lag(tolower(x$targeted),n=7L))), 1,x$ord)
  #
  # Adapting (i.e. 4.2.1) and phase out should be a 1
  #
  x$ord <- ifelse(x$who_code == '4.2.1' & 
                    x$measure_stage %in% c("phase-out","finish"),1,x$ord)
  #
  #
  # Closing (i.e. 4.2.2) and phase out should be a 1
  #
  x$ord <- ifelse(x$who_code == '4.2.2' & 
                    x$measure_stage %in% c("phase-out","finish"),2,x$ord)
  #
  # if measure stage is new, extension or modification, the score does not go down
  #
  x$ord <- ifelse(x$who_code %in% c('4.2.1','4.2.2') & 
                    x$measure_stage %in% c('new','modification','extension') &
                    x$ord < lag(x$ord),lag(x$ord),x$ord)
  #
  x <- replace_na(x, list(ord = 1))
  #
  # Clean up
  #
  x <- x %>%
    select(-c(opened_left_shut,shut_left_opened,bus_shut,bus_open))
  
  return(x)
}
