cleanVaccineData <- function(data, codes = country_codes){
  df <- data %>% rename(country = data.country) %>%
    mutate(date = ymd(date), country = as.factor(country))
  df <- df %>% group_by(date, country) %>%
    summarise(accept = mean(accept)) %>%
    ungroup()
  df <- left_join(df, select(country_codes, country, iso_code), on = "country")
}


cleanMeasuresData <- function(data, weeks = weeks, acceptance = acceptance){
  data <- data %>% mutate(country_territory_area = as.factor(country_territory_area),
                               admin_level = as.factor(admin_level),
                               who_category = as.factor(who_category),
                               who_measure = as.factor(who_measure),
                               date_start = ymd(date_start),
                               date_end = ymd(date_end))
  data <- data %>% filter(date_start <= date_end,
                          iso %in% acceptance$iso_code,
                          admin_level == "national",
                          who_category != "Biological measures" & who_category != "Drug-based measures")
  
  date0 <- weeks$start[1]
  return_data <- data %>% filter(measure_stage != "phase-out" & measure_stage != "finish") %>%
    filter(date_start < ymd(date0) & date_end > ymd(date0)) %>%
    group_by(iso, who_category, who_measure) %>%
    summarise(value = n(),
              req = sum(enforcement == "required")) %>%
    mutate(Date = date0) %>%
    rename(iso_code = iso)
  
  for(i in 2:nrow(weeks)){
    new_date <- weeks$start[i]
    new_data <- data %>% filter(measure_stage != "phase-out" & measure_stage != "finish") %>%
      filter(date_start < ymd(new_date) & date_end > ymd(new_date)) %>%
      group_by(iso, who_category, who_measure) %>%
      summarise(value = n(),
                req = sum(enforcement == "required")) %>%
      mutate(Date = new_date) %>%
      rename(iso_code = iso)
    return_data <- rbind(return_data, new_data)
  }
  
  return_data <- return_data %>% mutate(date = ymd(Date), presence = 1)
  
  return(return_data)
}