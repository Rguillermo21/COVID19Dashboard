
##############################
## LAST UPDATED: 2023-01-02 ##
##############################


if (!require ("plyr")) install.packages("plyr")
if (!require ("tidyverse")) install.packages("tidyverse")
if (!require ("XML")) install.packages("XML")
if (!require ("rvest")) install.packages("rvest")
if (!require ("plotly")) install.packages("plotly")
if (!require ("shinydashboard")) install.packages("tidyverse")
if (!require ("shiny")) install.packages("shiny")
if (!require ("leaflet")) install.packages("leaflet")
if (!require ("DT")) install.packages("DT")
if (!require ("hrbrthemes")) install.packages("hrbrthemes")
if (!require ("RColorBrewer")) install.packages("RColorBrewer")
if (!require ("rgdal")) install.packages("rgdal")
if (!require ("rworldxtra")) install.packages("rworldxtra")
if (!require ("rworldmap")) install.packages("rworldmap")
if (!require ("rgeos")) install.packages("rgeos")
if (!require ("lubridate")) install.packages("lubridate")
if (!require ("zoo")) install.packages("zoo")


library('plyr')
library('tidyverse')
library('XML')
library('rvest')
library('plotly')
library('shinydashboard')
library('shiny')
library('leaflet')
library('DT')
library('lubridate')
library('rgeos')
library('rworldmap')
library('rworldxtra')
library('rgdal')
library('RColorBrewer')
library("zoo")



# Create Functions ----
# Web scrape to read in HTML Tables

scrape_table <- function(x, header = TRUE) { # I was hungry
  read_html(x) %>% # read website html into XML document
    html_node("table") %>% # Rvest parses xml document for node "table"
    html_table(header = header) # Default value is TRUE
} # Rvest turns table into data.frame




###########################
## New data source as of ##
##     2022-12-27        ## 
###########################

## Western Sahara population data
ws = scrape_table("https://worldpopulationreview.com/countries/western-sahara-population", header = FALSE) %>%
  mutate(X2 = as.numeric(gsub(",", "", X2))) %>%
  filter(row_number() == 1) %>%
  select(X2)

### Data pre-processing ###

### 2022-12-26 NEW DATA SOURCE FOR IMPLEMENTATION

new_data = read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  mutate(date = as.Date(date)) %>%
  dplyr::rename(Date = date)


#paste0("c(",paste0(which(!colnames(new_data) %in% colnames(world_spdf@data)), sep = "", collapse = ",") , ")")
continent_code = c("North America", "South America", "Asia", "Africa", "Oceania", "Antarctica", "Europe", "World") ## Need to separate out 
class_code = c("High income", 
               "Upper middle income",
               "Lower middle income",
               "Low income")

## COVID AGGREGATED BY CONTINENTS 
## We can test map on this set since smaller obs // filter for current date
covid_continents = new_data %>%
  filter(location %in% continent_code)

## COVID AGGREGATED BY COUNTRIES
## I WANNA SEE REGIONS THOUGH // MORE VARIATION WITHIN COUNTRIES FOR MORE ACCURATE PREDICTIONS

covid_class = new_data %>%
  filter(location %in% class_code)

## BASE MAP DATA
Corona_new = new_data %>%
  filter(!location %in% c(continent_code, class_code, "European Union")) %>%
  mutate(NAME = location) %>%
  select(NAME, Date, 
         total_cases, new_cases, new_cases_smoothed, 
         total_deaths, new_deaths, new_deaths_smoothed, 
         total_tests, new_tests, new_tests_smoothed,
         total_vaccinations, new_vaccinations, new_vaccinations_smoothed,
         total_boosters,
         positive_rate,reproduction_rate,
         median_age, population_density, extreme_poverty, gdp_per_capita, handwashing_facilities, 
         life_expectancy, hospital_beds_per_thousand,
         population, human_development_index)


## IMPUTE MISSING VALUES/ROWS
## We are holding these values constant since we do not have much accurate information

### SVALBARD ###

## IF Svalbard is not in the NAME column, we add a row for the values
## IF Svalbard is in the NAME column but has no entries, we add a row corresponding to the date
## This is not a generalizable if else statement and only works here because the data for Svalbard is sparse
## Not a lot of official statistics are posted for Svalbard or Western Sahara

## We also run into the issue of scalability doing hard-coded imputations. 
##  We can make this more robust if we impute missing values for entries past 2022-01-11 with the current COVID count
##  By web scraping it from a different source then OWID
all_dates = seq(min(Corona_new$Date), max(Corona_new$Date), 1)
for (i in 1:length(all_dates)) {
  if (nrow(Corona_new %>% filter(NAME %in% "Western Sahara") %>% select(Date)) < length(all_dates)) {
    ifelse(all_dates[i] > as.Date("2020-04-20"),
           Corona_new <- Corona_new %>% add_row(NAME = "Western Sahara",
                                                Date = all_dates[i], 
                                                total_cases = 766, 
                                                total_deaths = 2,
                                                total_tests = 3943,
                                                median_age = 28.4,
                                                life_expectancy = 70.26,
                                                population = as.numeric(paste0(ws)),
                                                population_density = as.numeric(paste0(ws))/266000),
           Corona_new <- Corona_new %>% add_row(NAME = "Western Sahara",
                                                Date = all_dates[i], 
                                                total_cases = NA, 
                                                total_deaths = NA,
                                                median_age = 28.4,
                                                life_expectancy = 70.26,
                                                population = as.numeric(paste0(ws)),
                                                population_density = as.numeric(paste0(ws))/266000))}
  
  if (nrow(Corona_new %>% filter(NAME %in% "Svalbard") %>% select(Date)) < length(all_dates)) {
    ifelse(all_dates[i] > as.Date("2022-01-11"), 
           Corona_new <- Corona_new %>% 
             add_row(
               NAME = "Svalbard", 
               total_cases = 297, 
               total_deaths = 0,
               Date = all_dates[i],
               population = 2642), 
           ifelse(all_dates[i] > as.Date("2021-10-06"), 
                  Corona_new <- Corona_new %>% 
                    add_row(
                      NAME = "Svalbard", 
                      total_cases = 23, 
                      total_deaths = 0,
                      Date = all_dates[i],
                      population = 2642),
                  Corona_new <-  Corona_new %>% 
                    add_row(
                      NAME = "Svalbard", 
                      total_cases = NA, 
                      total_deaths = NA,
                      Date = all_dates[i],
                      population = 2642)))}
}


### If NA,  last observation carried forward
Corona_new <- Corona_new %>% 
  group_by(NAME) %>%
  mutate(total_vaccinations = na.locf(total_vaccinations, na.rm = FALSE),
         total_cases = na.locf(total_cases, na.rm = FALSE),
         total_deaths = na.locf(total_deaths, na.rm = FALSE))

### WESTERN SAHARA ###
### DATA ONLY FROM MOROCCAN GOVERNMENT CONTROLLED TERRITORIES: 
### https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Western_Sahara
### https://worldpopulationreview.com/countries/western-sahara-population

### BUT NOT FROM SAHRAWI ARAB DEMOCRATIC REPUBLIC CONTROLLED TERRITORIES


### Subset data and store into dir
corona_new_total <- Corona_new %>% select(-c( total_deaths, new_deaths, new_deaths_smoothed, 
                                              total_tests, new_tests, new_tests_smoothed,
                                              total_vaccinations, new_vaccinations, new_vaccinations_smoothed))
corona_deaths_total <- Corona_new %>% select(-c(total_cases, new_cases, new_cases_smoothed, 
                                                total_tests, new_tests, new_tests_smoothed,
                                                total_vaccinations, new_vaccinations, new_vaccinations_smoothed))
corona_vaccine_total <- Corona_new %>% select(-c(total_cases, new_cases, new_cases_smoothed, 
                                                 total_tests, new_tests, new_tests_smoothed,
                                                 total_deaths, new_deaths, new_deaths_smoothed))
## We will query these in pgadmin4 
## Breaking down the data is easier to deal with
write.csv(corona_new_total, file = "C:\\Users\\Raffy\\OneDrive\\Documents\\DATA SCIENCE PROJECTS\\Corona data\\corona_new_total.csv")
write.csv(corona_deaths_total, file = "C:\\Users\\Raffy\\OneDrive\\Documents\\DATA SCIENCE PROJECTS\\Corona data\\corona_new_deaths.csv")
write.csv(corona_vaccine_total, file = "C:\\Users\\Raffy\\OneDrive\\Documents\\DATA SCIENCE PROJECTS\\Corona data\\corona_new_vaccine.csv")
write.csv(Corona_new, file = "C:\\Users\\Raffy\\OneDrive\\Documents\\DATA SCIENCE PROJECTS\\Corona data\\corona_main.csv")
write.csv(covid_continents, file = "C:\\Users\\Raffy\\OneDrive\\Documents\\DATA SCIENCE PROJECTS\\Corona data\\corona_global.csv")



