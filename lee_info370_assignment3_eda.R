# import libraries
library(dplyr)
library(ggplot2)
library(rworldmap)
library(countrycode)
library(sqldf)

# imports the data
data <- read.csv("strava_activity.csv", stringsAsFactors = FALSE)

focus.data <- read.csv("focus_data.csv", stringsAsFactors = FALSE)

female.data <- read.csv("female_data.csv", stringsAsFactors = FALSE)

male.data <- read.csv("male_data.csv", stringsAsFactors = FALSE)

# function to calculate the mode of data
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# gets the average speed among all females
female.average.speed <- round(mean(female.data$average_speed), 2)

# gets the median speed among all females
female.median.speed <- round(median(female.data$average_speed), 2)

# gets the mode speed among all females
female.mode.speed <- round(mode(female.data$average_speed), 2)

# gets the range of speed among all females
female.range.speed <- range(female.data$average_speed)

# gets the average meters/sec for females
female.average.m.s <- round(mean(female.data$meters_per_sec), 2)

# gets the average heart rate for females during activity
female.heart.rate <- round(mean(female.data$average_heartrate), 2)

# gets the average distance for one lap (females)
female.average.distance <- round(mean(female.data$distance), 2)

# gets the median distance for one lap (females)
female.median.distance <- round(median(female.data$distance), 2)

# gets the mode distance for one lap (females)
female.mode.distance <- round(mode(female.data$distance), 2)

# gets the average speed among all males
male.average.speed <- round(mean(male.data$average_speed), 2)

# gets the median speed among all males
male.median.speed <- round(median(male.data$average_speed), 2)

# gets the mode speed among all males
male.mode.speed <- round(mode(male.data$average_speed), 2)

# gets the range of speed among all males
male.range.speed <- range(male.data$average_speed)

# gets the average meters/sec for males
male.average.m.s <- round(mean(male.data$meters_per_sec), 2)

# gets the average heart rate for males during activity
male.heart.rate <- round(mean(male.data$average_heartrate), 2)

# gets the average distance for one lap (males)
male.average.distance <- round(mean(male.data$distance), 2)

# gets the median distance for one lap (females)
male.median.distance <- round(median(male.data$distance), 2)

# gets the mode distance for one lap (females)
male.mode.distance <- round(mode(male.data$distance), 2)


