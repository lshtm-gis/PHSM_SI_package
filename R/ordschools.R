#' Compute the Ordinal Scope Scale for Schools
#'
#' This function read the output of the \code{ordmasks} function
#' and compute the Ordinal scope scale for Indicator 4.1.1 and 4.1.2.
#'
#' @param x Output file from previous run - \code{ordmasks} function
#'
#' @return Computed ordinal scale for Schools category
#'
#' @export
#' @examples
#' ordschools(x)
#'
ordschools <- function(x){
  #
  # ###########################################################
  # School coding not strictly following the 'defined logic document'
  # But using combination of what is stated in the target variable to
  # identify schools that are closed or opened.
  ################################################################
  #
  # Define a list of keywords in Schools target variable which are of interest
  # 
  all_target <- c('primary schools','secondary schools','post-secondary schools',
  'all schools','all education','kindergarten')
  #
  all_target2 <- c('primary schools','secondary schools','post-secondary schools')
  #
  all_target3 <- c('kindergarten','primary schools','secondary schools')
  #
  # Identify Schools that are shut - i.e. if who_code = 4.1.2, and the record's 
  # target is in the list of keywords above, then the school is closed.
  # e.g. if who_code = 4.1.2, target = 'primary schools', then it means
  # 'primary school' is closed
  # 
  x <- x %>% mutate(shut = ifelse(who_code == '4.1.2', 
                                              apply(x, 1, function(x) {
      paste(all_target[all_target %in% strsplit(x[6], ",", 
      fixed=TRUE)[[1]]], collapse=",")
    }), ''))
  #
  # Identify Schools that are open - i.e. if who_code = 4.1.1, and the record's 
  # target is in the list of keywords above, then the school is opened.
  # e.g. if who_code = 4.1.1, target = 'primary schools', then it means
  # 'primary school' is opened.
  # 
  x <- x %>% mutate(
    open = ifelse(who_code == '4.1.1', 
                  apply(x, 1, function(x) {
      paste(all_target[all_target %in% strsplit(x[6], ",", 
      fixed=TRUE)[[1]]], collapse=",")
    }), ''))
  #
  # For school closing (4.1.2), identify schools that remain open.
  # e.g. if who_code = 4.1.2 and target = 'primary schools', then it means
  # 'primary school' is closed while others are opened. So basically get 
  # difference between what is reported closed and list of keyword(excluding
  # all schools and all education)
  #
  x <- x %>% mutate(
    shut_left_opened = ifelse(who_code == '4.1.2' & !x$shut %in% c('all schools','all education'), 
                              apply(x, 1, function(x) {
      paste(all_target2[!all_target2 %in% strsplit(x[8], ",", 
      fixed=TRUE)[[1]]], collapse=",")
    }), ''))
  #
  # For school adapting (4.1.1), identify schools that remain shut.
  # e.g. if who_code = 4.1.1 and target = 'primary schools', then it means
  # 'primary school' is opened while others are shut. So basically get 
  # difference between what is reported opened and list of keyword(excluding
  # all schools and all education)
  #
  x <- x %>% mutate(opened_left_shut = ifelse(who_code == '4.1.1' & !x$open %in% c('all schools','all education'), 
                                                          apply(x, 1, function(x) {
      paste(all_target2[!all_target2 %in% strsplit(x[9], ",", 
      fixed=TRUE)[[1]]], collapse=",")
    }), ''))
  #
  # Get actual closed schools
  #
  x$sch_all_shut <- paste (x$shut,x$opened_left_shut, sep =',' )
  #
  # Get actual opened schools
  #
  x$sch_all_open <- paste (x$open,x$shut_left_opened, sep =',')
  #
  # End of data preparation
  #
  # Now to assign the ordinal code
  #
  # Assign 0 as default for all 4.1.1 and 4.1.2
  #
  x$ord <- ifelse(x$who_code %in% c('4.1.1','4.1.2'), 0,x$ord)
  #
  # Assign 4 if who_code = 4.1.2 and all schools or all education or all items in list of 
  # keyword is closed.
  #
  x$ord <- ifelse(x$who_code == '4.1.2' & 
                                (x$shut %in% c('all education','all schools') |
                                list.count(unique(x$shut)) == list.count(all_target3)), 4, x$ord)
  #
  # Assign 1 if who_code = 4.1.1 and all schools or all education or all items in list of 
  # keyword is opened.
  #
  x$ord <- ifelse(x$who_code == '4.1.1' & 
                                (x$open %in% c('all education','all schools') |
                                list.count(unique(x$open)) == list.count(all_target3)), 1, x$ord)
  #
  # Assign 3 if who_code = 4.1.2 and not all schools or all education are closed. i.e.
  # some schools are closed but not all.
  #
  x$ord <-ifelse(x$who_code == '4.1.2' &  
                               !x$shut %in% c('all schools','all education'),3,x$ord)
  #
  # Assign 3 if who_code = 4.1.1 and not all schools or all education are opened. i.e.
  # some schools are opened but not all.
  #
  x$ord <-ifelse(x$who_code == '4.1.1' &  
                               !x$open %in% c('all schools','all education'),3,x$ord)
  #
  # Assign 1 if only post-secondary are closed
  #
  x$ord <-ifelse(x$who_code %in% c('4.1.1', '4.1.2') & x$shut == 'post-secondary schools', 
                             1,x$ord)
  #
  # Assign 1 if measure stage is phase-out or finish and all schools or all education 
  # are shut
  # 
  x$ord <-ifelse(x$who_code == '4.1.2' & 
                   x$measure_stage %in% c("phase-out","finish") & 
                   x$shut %in% c('all schools','all education'), 
                 1,x$ord)
  #
  # Assign 1 if measure stage is phase-out or finish and target is 
  # primary schools and secondary schools
  #
  x$ord <-ifelse(x$who_code == '4.1.2' & 
                   x$measure_stage %in% c("phase-out","finish") & 
                   grepl('primary schools', tolower(x$targeted)) &
                   grepl('secondary schools', tolower(x$targeted)),1,x$ord)
  #
  # To adjust for fluctuations in coding, if previous measure stage and next measure stage is
  # phase-out or finish, and current measure stage is not new or introduction,
  # and current who code is same as previous who code, then current ordinal scale
  # is same as previous ordinal scale.
  #
  x$ord <- ifelse(lag(x$measure_stage, n=1L) %in% c("phase-out", "finish") &
                    lead(x$measure_stage, n=1L) %in% c("phase-out", "finish") &
                    !x$measure_stage %in% c("new","re-introduction") & 
                    x$who_code == '4.1.2' &
                    x$who_code == lag(x$who_code,n=1L), lag(x$ord,n=1L),x$ord)
  #
  # clean up
  #
  x <- x %>% select(-c(shut,open,opened_left_shut,shut_left_opened,sch_all_shut, sch_all_open))
  #     
  return(x)
}
#data3 <- ordschools(data2)
