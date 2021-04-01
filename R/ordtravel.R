#' Compute the Ordinal Scope Scale for International travel
#'
#' This function read the output of the \code{ordmove} function
#' and compute the E_scale, Q_scale and Ordinal scope scale for Indicators 5.2','5.3','5.5','5.7','5.8','5.9'.
#'
#' @param x Output file from previous run - \code{ordmove} function
#'
#' @return Computed ordinal scale for Travel category
#'
#' @export
#' @examples
#' ordtravel(x)
#'
ordtravel <- function(x){
  #
  # Assign default E0 for 5.2, 5.3, 5.7, 5.8, 5.9
  #
  x$E_ord<- ifelse(x$who_code %in% c('5.2','5.3','5.7','5.8','5.9'), 'E0','')
  #
  # Assign E1 if who code is any of 5.2, 5.3, 5.7, 5.8, 5.9 and target is single country
  # or single border and measure stage is not phase out or finish
  #
  x$E_ord <- ifelse(x$who_code %in% c('5.2','5.3','5.7','5.8','5.9') & 
                                 grepl(paste(c('single country','single border'),
                                 collapse = "|"),tolower(x$targeted)) & 
                                 !grepl(paste(c('phase out', 'finish'),
                                 collapse = "|"),tolower(x$measure_stage)), 
                               'E1',x$E_ord)
  #
  # Assign E2 if who code is any of 5.2, 5.3, 5.7, 5.8, 5.9 and target is multiple countries
  # or high risk countries or low risk countries or multiple borders or limited flights and 
  # measure stage is not finish.
  #
  x$E_ord <- ifelse(x$who_code %in% c('5.2','5.3','5.7','5.8','5.9') & 
                                 grepl(paste(c('multiple countries','high-risk countries',
                                 'low-risk countries','multiple borders','limited flights'),
                                             collapse = "|"),tolower(x$targeted)) & 
                                 !grepl('finish',tolower(x$measure_stage)), 'E2',x$E_ord)
  #
  # Assign E3 if who code is any of 5.2, 5.3, 5.7, 5.9 and target is all countries
  # or all borders or all travelers or all flights or non-residents and measure stage is not phase-out
  #
  x$E_ord <- ifelse(x$who_code %in% c('5.2','5.3','5.7','5.9') & 
                                 grepl(paste(c('all countries','all borders','all travellers','all flight','non-residents'),
                                             collapse = "|"),tolower(x$targeted)) &
                                 !x$measure_stage == 'phase-out',
                               'E3',x$E_ord) 
  #
  # Assign default Q0 for 5.3 and 5.5
  #
  x$Q_ord <- ifelse(x$who_code %in% c('5.3','5.5'), 'Q0','')
  #
  # Assign Q1 if who code is 5.3 or 5.5 and target is quarantine or test and target is single country or 
  # multiple countries or high risk countries or low risk countries
  #
  x$Q_ord <- ifelse(x$who_code %in% c('5.3','5.5') & 
                      grepl(paste(c('quarantine', 'pcr test', 'pcr-test', 'test', 'antibody'),
                                  collapse = "|"),tolower(x$targeted)) &
                                 grepl(paste(c('single country','multiple countries','high-risk countries',
                                 'low-risk countries'),
                                             collapse = "|"),tolower(x$targeted)),
                               'Q1',x$Q_ord)
  #
  # Assign Q2 if who code is 5.3 or 5.5 and target is quarantine or test and target is all countries or 
  # all travelers or borders or all borders and target is not single country or 
  # multiple countries or high risk countries or low risk countries
  #
  x$Q_ord <- ifelse(x$who_code %in% c('5.3','5.5') & 
                      grepl(paste(c('quarantine', 'pcr test', 'pcr-test', 'test', 'antibody'),
                                  collapse = "|"),tolower(x$targeted)) & 
                                 grepl(paste(c('all countries','all travellers','borders','all borders'),collapse = "|"),
                                 tolower(x$targeted)) &
                                 !grepl(paste(c('single country','multiple countries','high-risk countries',
                                 'low-risk countries'),
                                              collapse = "|"),tolower(x$targeted)), 
                               'Q2',x$Q_ord)
  
  #####################################################################
  # Below is to select the strictest measure impose in a day. For example
  # if for a particular day, there is Visa, Entry, Flight, Ferry and Border
  # restrictions. The strictest of these measures is used. This is done for
  # both E (E_max) and Q (Q_max).
  #
  # The E_max and Q_max value is then used to assign the ordinal 
  # scale score E_ord and Q_ord.
  #####################################################################
  #
  # Starting with E case, i.e. 5.2, 5.3, 5.7, 5.8, 5.9
  #
  x2 <- x %>% filter(who_code %in% c('5.2','5.3','5.7','5.8','5.9')) %>%
    select(-Q_ord) %>%
    mutate(group = case_when(who_code == '5.2' ~ 'Visas',
                             who_code == '5.3' ~ 'Entry',
                             who_code == '5.7' ~ 'Flights',
                             who_code == '5.8' ~ 'Ferry',
                             who_code == '5.9' ~ 'Border',
                             TRUE ~ 'others')) %>% 
    pivot_wider(names_from = group, values_from = E_ord, values_fill=list(value=0)) 

  x2$flight_num <- NA
  x2$border_num <- NA
  x2$ferry_num <-NA
  x2$visas_num <- NA
  x2$entry_num <- NA
  
  x2$flight_num[x2$Flights == "E0"] <- 0
  x2$flight_num[x2$Flights == "E1"] <- 1
  x2$flight_num[x2$Flights == "E2"] <- 2
  x2$flight_num[x2$Flights == "E3"] <- 3
  
  x2$border_num[x2$Border == "E0"] <- 0
  x2$border_num[x2$Border == "E1"] <- 1
  x2$border_num[x2$Border == "E2"] <- 2
  x2$border_num[x2$Border == "E3"] <- 3
  
  x2$ferry_num[x2$Ferry == "E0"] <- 0
  x2$ferry_num[x2$Ferry == "E1"] <- 1
  x2$ferry_num[x2$Ferry == "E2"] <- 2
  x2$ferry_num[x2$Ferry == "E3"] <- 3
  
  x2$visas_num[x2$Visas == "E0"] <- 0
  x2$visas_num[x2$Visas == "E1"] <- 1
  x2$visas_num[x2$Visas == "E2"] <- 2
  x2$visas_num[x2$Visas == "E3"] <- 3
  
  x2$entry_num[x2$Entry == "E0"] <- 0
  x2$entry_num[x2$Entry == "E1"] <- 1
  x2$entry_num[x2$Entry == "E2"] <- 2
  x2$entry_num[x2$Entry == "E3"] <- 3
  
  x2 <- x2 %>% 
    fill(flight_num) %>%
    fill(border_num) %>%
    fill(ferry_num) %>%
    fill(visas_num) %>%
    fill(entry_num)
  
#  x2$visas_num <- revalue(x2$Visas,
#                          c(na = 0, "E0"=1, "E1"=2, "E2"=3, "E3"=4))
  
  x2 <- replace_na(x2, list(flight_num = 0))
  x2 <- replace_na(x2, list(border_num = 0))
  x2 <- replace_na(x2, list(ferry_num = 0))
  x2 <- replace_na(x2, list(visas_num = 0))
  x2 <- replace_na(x2, list(entry_num = 0))
 
  x2$E_max <- pmax(x2$flight_num, x2$border_num,x2$ferry_num,x2$visas_num,x2$entry_num) 
  
  x2 <- x2 %>% mutate(
    E_ord = case_when(E_max  == '0' ~ 'E0',
                      E_max  == '1' ~ 'E1',
                      E_max  == '2' ~ 'E2',
                      E_max  == '3' ~ 'E3',
                            TRUE ~ 'Others'))
  x2 <- x2 %>%
    select(c(who_id:ord, E_ord,E_max))
  #
  # Then Q case, i.e 5.3 and 5.5
  #
  y2 <- x %>% filter(who_code %in% c('5.3','5.5')) %>%
    select(-E_ord) %>%
    mutate(group = case_when(who_code == '5.3' ~ 'Q53',
                             who_code == '5.5' ~ 'Q55',
                             TRUE ~ 'Others')) %>%
    pivot_wider(names_from = group, values_from = Q_ord, values_fill=list(value=0)) 

  
  y2$f3_num <- NA
  y2$f5_num <- NA
  
  y2$f3_num[y2$Q53 == "Q0"] <- 0
  y2$f3_num[y2$Q53 == "Q1"] <- 1
  y2$f3_num[y2$Q53 == "Q2"] <- 2
  
  y2$f5_num[y2$Q55 == "Q0"] <- 0
  y2$f5_num[y2$Q55 == "Q1"] <- 1
  y2$f5_num[y2$Q55 == "Q2"] <- 2
  
  # remove comment if this need filling
  #y2 <- y2 %>% 
   # fill(f3_num) %>%
   # fill(f5_num) 
  
  y2 <- replace_na(y2, list(f3_num = 0))
  y2 <- replace_na(y2, list(f5_num = 0))
  
  y2$Q_max <- pmax(y2$f3_num,y2$f5_num) 
  
  y3 <- y2 %>% mutate(
    Q_ord = case_when(Q_max  == '0' ~ 'Q0',
                      Q_max  == '1' ~ 'Q1',
                      Q_max  == '2' ~ 'Q2',
                      TRUE ~ 'Others'))
  
  y3 <- y3 %>%
    select(c(who_id:ord, Q_ord, Q_max))
  #
  # special preparation for 5.3 because it appears under E scale and Q scale
  #
  y3_53 <- y3 %>%
    filter(who_code == '5.3')
  
  x2_53 <- x2 %>%
    filter(who_code == '5.3')
  
  all_53 <- full_join(y3_53, x2_53)
  
  y3 <- y3 %>%
    filter(!who_code == '5.3')
  
  x2 <- x2 %>%
    filter(!who_code == '5.3')
  
  y3$E_ord <- NA
  y3$E_max <- NA
  x2$Q_ord <- NA
  x2$Q_max <- NA
  
  travel <- rbind(y3,x2,all_53)
  #travel <- replace_na(travel, list(Q_ord = 0))

  a2 <- x %>% filter(!who_code %in% c('5.2','5.7','5.8','5.9','5.5','5.3'))
  
  a2$E_max <- NA
  a2$Q_max <- NA
  #a2 <- replace_na(a2, list(E_max = 0))
  
  x <- rbind(a2,travel)
  
  x <- x[order(as.Date(x$date_start, format="%Y-%m-%d")),]
  #
  #
  # ???? ordinal Scale for Travel ????
  #
  #x$ord <- ifelse(x$who_code %in% c('5.2','5.7','5.8','5.9','5.5','5.3') & x$E_max == 3,6,x$ord)
  #
  #
  #x$ord <- ifelse(x$who_code %in% c('5.2','5.7','5.8','5.9','5.5','5.3') &
  #                  (x$E_max == 2 & x$Q_max == 'Q2'), 5,x$ord)
  #
  ## Assign 6
  x$ord <- ifelse(x$who_code %in% c('5.2','5.7','5.8','5.9','5.5','5.3') &
                    (x$E_ord == 'E3' & x$Q_ord == 'Q0' |
                       x$E_ord == 'E3' & x$Q_ord == 'Q1' |
                       x$E_ord == 'E3' & x$Q_ord == 'Q2' |
                       x$E_ord == 'E3' & is.na(x$Q_ord)), 6,x$ord)
  #
  ## Assign 5
  x$ord <- ifelse(x$who_code %in% c('5.2','5.7','5.8','5.9','5.5','5.3') &
                    (x$E_ord == 'E2' & x$Q_ord == 'Q2'), 5,x$ord)
  #
  ## Assign 4
  x$ord <- ifelse(x$who_code %in% c('5.2','5.7','5.8','5.9','5.5','5.3') &
                    (x$E_ord == 'E0' & x$Q_ord == 'Q2' |
                       x$E_ord == 'E1' & x$Q_ord == 'Q2'|
                       is.na(x$E_ord) & x$Q_ord == 'Q2'), 4,x$ord)
  #
  ## Assign 3
  x$ord <- ifelse(x$who_code %in% c('5.2','5.7','5.8','5.9','5.5','5.3') &
                   (x$E_ord == 'E2' & x$Q_ord == 'Q1'), 3,x$ord)
  #
  ## Assign 2
  x$ord <- ifelse(x$who_code %in% c('5.2','5.7','5.8','5.9','5.5','5.3') &
                    (x$E_ord == 'E1' & x$Q_ord == 'Q1' |
                       x$E_ord == 'E2' & x$Q_ord == 'Q0' |
                       x$E_ord == 'E2' & is.na(x$Q_ord)) , 2,x$ord)
  #
  ## Assign 1
  x$ord <- ifelse(x$who_code %in% c('5.2','5.7','5.8','5.9','5.5','5.3') &
                    (x$E_ord == 'E1' & x$Q_ord == 'Q0' |
                       x$E_ord == 'E0' & x$Q_ord == 'Q1' |
                       x$E_ord == 'E1' & is.na(x$Q_ord) |
                       is.na(x$E_ord) & x$Q_ord == 'Q1'), 1,x$ord)
  #
  ## Assign 0
  x$ord <- ifelse(x$who_code %in% c('5.2','5.7','5.8','5.9','5.5','5.3') &
                    (x$E_ord == 'E0' & x$Q_ord == 'Q0' |
                       x$E_ord == 'E0' & is.na(x$Q_ord)  |
                       is.na(x$E_ord) & x$Q_ord == 'Q0'), 0,x$ord)
  
  return(x)
}
#
#data7 <- ordtravel(data6)
#data_travel_f<- data7 %>%
#  filter(who_code %in% c('5.2','5.7','5.8','5.9','5.5','5.3'))
#
