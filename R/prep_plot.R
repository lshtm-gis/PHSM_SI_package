#' Prepares the data in the appropriate format 
#'
#' This function reads the SI value and fill it
#' for all dates, starting from the minimum date in the data
#'
#' @param x CSV file with SI value
#'
#' @return Final csv file
#'
#' @export
#' @examples
#' prep_plot(x)
#'
prep_plot <- function(x,country){
  #
  # Get the minimum date in data and create a DF of dates
  # from min date to current date
  #
  x$date_start <- lubridate::ymd(x$date_start)
  x <- x[order(as.Date(x$date_start, format="%Y-%m-%d")),]
  
  start_date <- min(x$date_start, na.rm = TRUE)
  today <- Sys.Date()
  n_days <- interval(start_date,today)/days(1)
  date <- start_date + days(0:n_days)
  date <- as.data.frame(date)
  
  date <- date %>% dplyr::rename(date_start = date)
  print(paste0("Start date in data is: ", start_date))
  
  y <- x %>%
    filter (country_territory_area == country) %>%
    group_by(date_start,category) %>%
    pivot_wider(names_from = category, values_from = SI)
  
  y$date_start <- lubridate::ymd(y$date_start)
  y = y[order(as.Date(y$date_start, format="%Y-%m-%d")),]
  
  y <- arrange(y, date_start) %>%
    right_join(date)
  
  y = y[order(as.Date(y$date_start, format="%Y-%m-%d")),]
  
  cols <- c(Mask = NA_real_, School = NA_real_, Business = NA_real_,
            Gatherings = NA_real_, Movements = NA_real_, Travel = NA_real_)
  
  y <- add_column(y, !!!cols[setdiff(names(cols), names(y))])
  
  y <- y %>%
    group_by(date_start, country_territory_area) %>%
    summarise(Mask =
                ifelse(!is.null(Mask), max(as.numeric(Mask)),""),
              Business =
                ifelse(!is.null(Business), max(as.numeric(Business)),""),
              Gathering =
                ifelse(!is.null(Gatherings), max(as.numeric(Gatherings)),""),
              School =
                ifelse(!is.null(School), max(as.numeric(School)),""),
              Movement =
                ifelse(!is.null(Movements), max(as.numeric(Movements)),""),
              Travel =
                ifelse(!is.null(Travel), max(as.numeric(Travel)),""),
              Binary =
                ifelse(!is.null(binary), max(as.numeric(binary)),""),
              Ordinal =
                ifelse(!is.null(ordinal), max(as.numeric(ordinal)),""))
  ########################
  write.csv(y,file='y.csv', row.names=FALSE)
  y <- read.csv('y.csv')
  #######################
  y <- y %>%
    dplyr::arrange(date_start) %>%
    fill(country_territory_area,.direction = "downup") %>%
    fill(Binary) %>%
    fill(Ordinal) %>%
    fill(School) %>%
    fill(Gathering) %>%
    fill(Movement) %>%
    fill(Business) %>%
    fill(Mask) %>%
    fill(Travel) %>%
    replace(is.na(.),0)
  #
  # global index = average score for all categories
  #
  y$global_index <- round(rowMeans(y[,3:8]))
  
  y <- y %>%
    select(date_start, country_territory_area, global_index, Mask, School,
            Business, Gathering, Movement, Travel)
  return(y)
}

