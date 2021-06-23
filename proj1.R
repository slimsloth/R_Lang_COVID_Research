library(tidyverse)

covid <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
demographics <- read_csv("demographics.csv")
bed <- read_csv("hospitalbeds.csv")
covid <- covid %>% select(-Lat, -Long) %>% pivot_longer(!c(`Country/Region`,`Province/State`), names_to = 'date', values_to = 'cases')
