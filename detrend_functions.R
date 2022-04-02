### This script holds the Detrend Functions used in 2 Figure 2 for Hitt et all 2022s

detrend_coral <- function(dataframe, coral, proxy){
  dataframe_2 <- dataframe %>%
    dplyr::filter(Coral_name == coral) %>%
    select(age, proxy)
  dataframe_3 <- dataframe_2 %>%
    select(proxy)
  detrended <- detrend(as.numeric(unlist(dataframe_3)))
  new_frame <- data.frame(dataframe_2$age, detrended) %>%
    mutate(Coral_name = coral)
  return(new_frame)
}

detrend_coral_interp <- function(dataframe, coral, proxy, time_vec){
  dataframe_2 <- dataframe %>%
    dplyr::filter(Coral_name == coral) %>%
    select(age, proxy)
  dataframe_3 <- dataframe_2 %>%
    select(proxy)
  detrended <- detrend(as.numeric(unlist(dataframe_3)))
  xi <- time_vec %>%
    filter(age_int >= min(dataframe_2[,1])) %>% 
    filter(age_int <= max(dataframe_2[,1])) 
  output <- interp1(as.numeric(dataframe_2$age), as.numeric(detrended), xi[[1]], method = c("linear"))
  new_frame <- data.frame(xi, output) %>%
    mutate(Coral_name = coral)
  colnames(new_frame) <- c("age_int", "d15n", "Coral_name")
  return(new_frame)
}