sevindex <- function(x,country = NULL){
  data0 <- dataprep(x,country)
  data1 <- binaryindex(data0)
  data2 <- ordmasks(data1)
  data3 <- ordschools(data2)
  data4 <- ordbusiness(data3)
  data5 <- ordgather(data4)
  data6 <- ordmove(data5)
  data7 <- ordtravel(data6)
  

  # Get only required columns
  data8 <- data7 %>%
    filter(who_code %in% c('1.4', '4.1.1', '4.1.2', '4.2.1', '4.2.2',
                           '4.3.1', '4.3.2', '4.3.3', '4.3.4', '4.5.1','4.5.2','4.5.3','4.5.4',
                           '5.2','5.3','5.5','5.7','5.8','5.9'))
  data9 <- calc_sev(data8)

  return(data9)
}
#aa2 <- sevindex('mistress_202103161.csv')


