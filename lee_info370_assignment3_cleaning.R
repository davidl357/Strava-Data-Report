# import libraries
library(dplyr)
library(ggplot2)
library(rworldmap)
library(countrycode)

# imports the data
data <- read.csv("strava_activity.csv", stringsAsFactors = FALSE)

# distance is in meters, speed in m/s, both for 1 lap
# gathers the columns of data for sex, distance per lap and speed per lap while getting rid of entries that are missing or with values of 0
focus.data <- select(data, athlete.sex, distance, elapsed_time, average_speed, type, average_heartrate, athlete.country) %>%
  filter(athlete.sex != " ") %>%
  filter(distance > 0.0) %>%
  filter(average_speed > 0.0) %>%
  filter(average_heartrate > 0.0) %>%
  filter(elapsed_time > 0.0) %>%
  filter(athlete.country != "") %>%
  mutate(meters_per_sec = distance / elapsed_time)

# removes country data that has unusable country data
focus.data <- focus.data[-c(786, 1281, 1345, 10, 1366, 1441, 506, 139, 809, 1217, 1447, 1742, 1266, 1226, 1189), ]

# gets the iso3 codes for each athlete's country for mapping
codes <- focus.data$athlete.country
athlete.countries <- countrycode(codes, 'country.name', 'iso3c')
focus.data$iso3c <- athlete.countries

# creates objects for the mapping function
locdf <- data.frame(country = focus.data$iso3c, Frequencies = 1)

# groups the country data by country and frequencies
cleanlocdf <- group_by(locdf, country) %>%
  summarize(
    Frequencies = sum(Frequencies)
  )

# removes rows with NA values
cleanlocdf <- filter(cleanlocdf, country != " ")

locmap <- joinCountryData2Map(cleanlocdf, joinCode = "ISO3", nameJoinColumn = "country")

# gets separates the data for male and female omitting any variables that are blank
female.data <- filter(focus.data, athlete.sex == "F")

male.data <- filter(focus.data, athlete.sex == "M")

write.csv(focus.data, file = "focus_data.csv")

write.csv(female.data, file = "female_data.csv")

write.csv(male.data, file = "male_data.csv")

write.csv(cleanlocdf, file = "country_freq.csv")

