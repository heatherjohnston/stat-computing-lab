# Gather data for lab

setwd("/Users/hajohns/Desktop/stat_306/stat-computing-lab/lab02/")

library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(jsonlite)
library(ggpubr)


source("code/utils.R")
source("code/load.R")
source("code/clean.R")


# NPI file:
# download from https://orlagh.blob.core.windows.net/release/release_2021_12_07.xlsx
# or available on web page https://www.who.int/emergencies/diseases/novel-coronavirus-2019/phsm 
measures_orig <- read_xlsx("data/UN_PHSM/release_2021_11_22.xlsx")

# included in zip file (not too big)
country_codes <- read.csv("data/Global Trends and Impact Study/country_region_codes.csv")

# downloaded directly from internet
covid_cases <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
weeks <- defineWeeks()


## The below lines can be uncommented and run to call the API and save data locally
acceptance_orig <- loadAcceptanceDataWeeks(weeks = weeks)
write.csv(vaccine_data_weeks, "data/vaccine_data_weeks.csv")

## If the data is already available locally, read it in with the line below instead
# acceptance_orig <- read.csv("data/vaccine_data_weeks.csv")

acceptance <- cleanVaccineData(acceptance_orig)
measures <- cleanMeasuresData(data = measures_orig, weeks = weeks, acceptance = acceptance)

# join all the data together
vaccine_data <- covid_cases %>% 
  filter(iso_code %in% acceptance$iso_code) %>%
  rename(cases_pm = new_cases_smoothed_per_million,
         vaxxed_ph = people_vaccinated_per_hundred,
         deaths_pm = new_deaths_smoothed_per_million,
         tests_pt = new_tests_smoothed_per_thousand,
         country = location) %>%
  select(stringency_index, cases_pm, vaxxed_ph, deaths_pm,
         date, iso_code, continent, country, positive_rate,
         population) %>%
  mutate(date = ymd(date)) %>%
  inner_join(acceptance, on = c("iso_code", "date")) %>%
  distinct()

vaccine_data <- vaccine_data %>%
  filter(cases_pm >= 0,
         deaths_pm >= 0)

vaccine_data <- vaccine_data %>% 
  mutate(death_group = factor(ntile(deaths_pm, 3), labels = c("low", "average", "high")),
         cases_group = factor(ntile(cases_pm, 3), labels = c("low", "average", "high")))

measures_data <- vaccine_data %>%
  inner_join(measures, by = c("date", "iso_code")) %>%
  distinct()

measures_wide <- measures_data %>%
  mutate(who_measure = str_replace_all(who_measure, ",", "")) %>%
  mutate(who_measure = str_replace_all(who_measure, "-", "")) %>%
  mutate(who_measure = str_replace_all(who_measure, " ", "_")) %>%
  mutate(value = presence + req) %>%
  select(-who_category, -req) %>%
  pivot_wider(values_from = "value", names_from = "who_measure", values_fill = 0,
              values_fn = function(x) 1)

measure_cols <- measures_data %>%
  mutate(who_measure = str_replace_all(who_measure, ",", "")) %>%
  mutate(who_measure = str_replace_all(who_measure, "-", "")) %>%
  mutate(who_measure = str_replace_all(who_measure, " ", "_")) %>%
  select(who_measure) %>%
  unique()

