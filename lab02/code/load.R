# be sure to load packages and define date range variables

defineWeeks <- function(date0 = ymd("20201221"), n_weeks = 48){
  weeks <- data_frame(start = rep(ymd("2020-01-01"), n_weeks),
                      end = rep(ymd("2020-01-01"), n_weeks),
                      id = 1:n_weeks)
  for(i in 1:n_weeks){
    weeks$start[i] <- ymd(date0 + duration(7*(i-1), unit = "days"))
    weeks$end[i] <- ymd(weeks$start[i] + duration(6, unit = "days"))
  }
  weeks$start <- format(weeks$start, "%Y%m%d") 
  weeks$end <- format(weeks$end, "%Y%m%d") 
  return(weeks)
}

loadAcceptanceDataWeeks <- function(weeks = weeks){
  daterange0 <- paste(weeks$start[1], weeks$end[1], sep = "-")
  va_data <- getAPIData(indicator = "vaccine_acpt", daterange = daterange0)
  va_data <- va_data %>% group_by(data.country) %>%
    summarise(accept = weighted.mean(data.smoothed_vu, data.sample_size),
              date = weeks$start[1])

  for(i in 2:nrow(weeks)){
    new_start <- weeks$start[i]
    new_range <- paste(weeks$start[i], weeks$end[i], sep = "-")
    if(ymd(weeks$start[i]) < ymd("2021-05-20")){
      new_data <- getAPIData(indicator = "vaccine_acpt", daterange = new_range)
      new_data <- new_data %>% group_by(data.country) %>%
        summarise(accept = weighted.mean(data.smoothed_vu, data.sample_size),
                  date = new_start)
      va_data <- rbind(va_data, new_data)
    }
    if(ymd(weeks$end[i]) > ymd("2021-05-20")){
      new_data <- getAPIData(indicator = "appointment_or_accept_covid_vaccine", daterange = new_range)
      new_data <- new_data %>% group_by(data.country) %>%
        summarise(accept = weighted.mean(data.smoothed_pct_appointment_or_accept_covid_vaccine, data.sample_size),
                  date = new_start)
      va_data <- rbind(va_data, new_data)
    }
  }
  return(va_data)
}



