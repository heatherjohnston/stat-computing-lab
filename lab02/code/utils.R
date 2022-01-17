getAPIData <- function(indicator, country = "%", daterange){
  # indicator should be one of the following:
  #     "vaccine_acpt" # (pre-wave11) those who would get a vaccine / those who haven't been vaccinated and don't have an appointment
  #     "modified_acceptance" # (pre-wave11) vaccinated or would get vaccinated / everyone
  #     "appointment_have" (pre-wave11) have an appt / everyone
  #     "appointment_tried" (pre-wave11) have tried to get an appt
  #     "vaccinated_appointment_or_accept" # vaccinated or have appointment or would accept / everyone (though this seems to be mistated in the documentation)
  #     "appointment_or_accept_covid_vaccine" # have appointment or would accept / unvaccinated
  #     "covid_vaccine" # those who have gotten a vaccine
  # daterange should be a string in YYYYMMDD-YYYYMMDD format
  # country should be a string with a country name (see file /data/Global Trends and Impact Study/country_region_codes.csv)
  path <- "https://covidmap.umd.edu/api/resources?indicator=%s&type=smoothed&country=%s&daterange=%s"
  path <- sprintf(path, indicator, country, daterange)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  results <- fromJSON(response, flatten = TRUE) %>% data.frame()
  return(results)
}