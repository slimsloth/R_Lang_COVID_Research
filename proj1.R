library(tidyverse)

covid_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
demographics <- read_csv("demographics.csv")
beds <- read_csv("hospitalbeds.csv")

# Clean data up
covid_cases <- covid_cases %>% select(-Lat, -Long) %>% pivot_longer(!c(`Country/Region`,`Province/State`), names_to = 'date', values_to = 'cases')
covid_cases <- covid_cases %>% group_by(`Country/Region`,`date`) %>% summarise(cases=sum(cases))
covid_cases <- covid_cases %>% group_by(`Country/Region`) %>% summarise(cases=max(cases))
covid_cases <- covid_cases %>% rename(`Country` = `Country/Region`)

covid_deaths <- covid_deaths %>% select(-Lat, -Long) %>% pivot_longer(!c(`Country/Region`,`Province/State`), names_to = 'date', values_to = 'deaths')
covid_deaths <- covid_deaths %>% group_by(`Country/Region`,`date`) %>% summarise(deaths=sum(deaths))
covid_deaths <- covid_deaths %>% group_by(`Country/Region`) %>% summarise(deaths=max(deaths))
covid_deaths <- covid_deaths %>% rename(`Country` = `Country/Region`)

beds.tidy <- beds %>% group_by(Country) %>% summarize(Year=max(Year))
beds.tidy <- beds.tidy %>% left_join(beds)

demographics <- demographics %>% rename(`Country` = `Country Name`)
demographics <- demographics %>% select(-"Series Name", -`Country Code`) %>% pivot_wider(names_from = "Series Code", values_from = "YR2015")

joined_data <- covid_cases %>% inner_join(beds.tidy) %>% inner_join(covid_deaths) %>% inner_join(demographics) %>% select(-Year) %>% rename(beds = `Hospital beds (per 10 000 population)`)
joined_data <- joined_data %>% mutate(deathrate = deaths/cases) %>% view()
joined_data <- joined_data %>% select(Country, deathrate, cases, beds, SP.DYN.LE00.IN, SP.URB.TOTL, SP.POP.TOTL, SP.POP.80UP.FE, SP.POP.80UP.MA, SP.POP.1564.MA.IN, SP.POP.1564.FE.IN, SP.POP.0014.MA.IN, SP.POP.0014.FE.IN, SP.DYN.AMRT.FE, SP.DYN.AMRT.MA, SP.POP.TOTL.FE.IN, SP.POP.TOTL.MA.IN, SP.POP.65UP.FE.IN, SP.POP.65UP.MA.IN)
joined_data %>% view()