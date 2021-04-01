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
  # ###########################################################
  # Business coding not strictly following the 'defined logic document'
  # But using combination of what is stated in the target variable, to
  # identify businesses that are closed or opened.
  ################################################################
  #
  # Define a list of keywords in Businesses target variable which are of interest
  #
  all_target_b <- c('hospitality','entertainment','fitness','recreation','industry','retail','offices',
                    'beauty','tourism','religion','health','venues','non-essential businesses')
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
                       #grepl('non-essential businesses', lag(tolower(x$targeted),n=1L)) |
                       #grepl('non-essential businesses', lag(tolower(x$targeted),n=1L)) |
                       #grepl('non-essential businesses', lag(tolower(x$targeted),n=1L)) |
                       #grepl('non-essential businesses', lag(tolower(x$targeted),n=1L)), 1,x$ord))
  
                    #(lag(x$targeted, n=1L) %in% c('all businesses', 'non-essential businesses') |
                    #lag(x$targeted, n=2L) %in% c('all businesses', 'non-essential businesses') |
                    #lag(x$targeted, n=3L) %in% c('all businesses', 'non-essential businesses') |
                    #lag(x$targeted, n=4L) %in% c('all businesses', 'non-essential businesses')),1, x$ord)
                    #(!x$bus_shut %in% c('all businesses', 'non-essential businesses')), 2, x$ord)
  #
  #grepl(paste(c('large'),collapse = "|"),
  #tolower(x$targeted)) ,1,x$ord)
#grepl('primary schools', tolower(x$targeted))
  
  # paste shut + opened but shut to get what is actually shut
  #
  #business$all_shut <- paste (business$shut,business$shut_left_opened, sep =',')
  #
  #
  # paste open + shut_left_opened to get what is actually open
  #
  #business$all_open <- paste (business$open,business$opened_left_shut, sep =',')
  #
  #
  #business$all_shut[business$all_open == 'non-essential businesses'] <- ' '
  #
  # clean up
  #
  x <- x %>%
    select(-c(opened_left_shut,shut_left_opened,bus_shut,bus_open))
  
  return(x)
}
#data4<- ordbusiness(data3)
#data_business_a <- data_a %>%
#  filter(who_code %in% c('4.2.1','4.2.2'))
#
#data_business_a= data_business_a[order(as.Date(data_business_a$date_start, format="%Y-%m-%d")),]


#albania_business <- ordbusiness(data_business_a)

#write.csv(albania_business, 'albania_business.csv', row.names = FALSE)
