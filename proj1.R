library(tidyverse)
library(ggplot2)

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
beds.tidy <- beds.tidy %>% mutate(Country = replace(Country, Country == "Republic of Korea", "Korea, South"))
beds.tidy <- beds.tidy %>% mutate(Country = replace(Country, Country == "Iran (Islamic Republic of)", "Iran"))
beds.tidy <- beds.tidy %>% mutate(Country = replace(Country, Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))

demographics <- demographics %>% rename(`Country` = `Country Name`)
demographics <- demographics %>% select(-"Series Name", -`Country Code`) %>% pivot_wider(names_from = "Series Code", values_from = "YR2015")
demographics <- demographics %>% mutate(SP.POP.80UP = SP.POP.80UP.FE + SP.POP.80UP.MA)
demographics <- demographics %>% mutate(SP.POP.1564.IN = SP.POP.1564.FE.IN + SP.POP.1564.MA.IN)
demographics <- demographics %>% mutate(SP.POP.0014.IN = SP.POP.0014.FE.IN + SP.POP.0014.MA.IN)
demographics <- demographics %>% select(Country, SP.DYN.LE00.IN, SP.URB.TOTL, SP.POP.TOTL, SP.POP.80UP, SP.POP.1564.IN, SP.POP.0014.IN)
demographics <- demographics %>% mutate(Country = replace(Country, Country == "Korea, Rep.", "Korea, South"))
demographics <- demographics %>% mutate(Country = replace(Country, Country == "Iran, Islamic Rep.", "Iran"))

joined_data <- covid_cases %>% inner_join(beds.tidy) %>% inner_join(covid_deaths) %>% inner_join(demographics) %>% select(-Year) %>% rename(beds = `Hospital beds (per 10 000 population)`)
joined_data <- joined_data %>% mutate(deathrate = deaths/cases)
joined_data <- joined_data %>% relocate(deathrate, .before = cases) %>% select(-deaths)


# PART 2
# Dependent variable: deathrate
# Predictor variables:
# 1. cases
# 2. beds
# 3. SP.DYN.LE00.IN
# 4. SP.URB.TOTL
# 5. SP.POP.TOTL
# 6. SP.POP.80UP
# 7. SP.POP.1564.IN
# 8. SP.POP.0014.IN

mod <- lm(data=joined_data, deathrate~SP.POP.80UP+beds)
mycf <- coef(mod)
ggplot(data=joined_data)+geom_point(aes(x=SP.POP.80UP, y=deathrate)) + geom_abline(intercept=mycf[1], slope=mycf[2])
ggplot(data=joined_data, aes(x=cases, y=deathrate)) + geom_point()
