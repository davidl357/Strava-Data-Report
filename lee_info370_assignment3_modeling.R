# import libraries
library(dplyr)
library(ggplot2)
library(rworldmap)
library(countrycode)

# imports the data
data <- read.csv("strava_activity.csv", stringsAsFactors = FALSE)

focus.data <- read.csv("focus_data.csv", stringsAsFactors = FALSE)

female.data <- read.csv("female_data.csv", stringsAsFactors = FALSE)

male.data <- read.csv("male_data.csv", stringsAsFactors = FALSE)

par(mfrow = c(1 ,2))

# linear regression of female distance per lap and speed per lap
plot(female.data$distance, female.data$average_speed, main = "Female Speed & Distance\n Linear Regression", 
     xlab = "Distance of One Lap (Meters)", ylab = "Average Speed", col = "blue", xlim = c(0, 10000), ylim = c(0, 12),
     abline(lm(female.data$average_speed~female.data$distance)))

# linear regression of male distance per lap and speed per lap
plot(male.data$distance, male.data$average_speed, main = "Male Speed & Distance\n Linear Regression", 
     xlab = "Distance of One Lap (Meters)", ylab = "Average Speed", col = "blue", xlim = c(0, 10000), ylim = c(0, 12),
     abline(lm(male.data$average_speed~male.data$distance)))
     
# histogram of female average speed
ggplot(data = female.data) +
  geom_histogram(mapping = aes(x = female.data$average_speed), bins = 20) +
  labs(title = "Distribution of Average Speed for Females (1 Lap)", x = "Speed (Meters)", y = "Frequency")

# histogram of male average speed
ggplot(data = male.data) +
  geom_histogram(mapping = aes(x = male.data$average_speed), bins = 20) +
  labs(title = "Distribution of Average Speed for Males (1 Lap)", x = "Speed (Meters)", y = "Frequency")

# histogram of female distance
ggplot(data = female.data) +
  geom_histogram(mapping = aes(x = female.data$distance), bins = 20) +
  labs(title = "Distribution of Distance for Females (1 Lap)", x = "Distance (Meters)", y = "Frequency")
       
# histogram of male distance
ggplot(data = male.data) +
  geom_histogram(mapping = aes(x = male.data$distance), bins = 20) +
  labs(title = "Distribution of Distance for Males (1 Lap)", x = "Distance (Meters)", y = "Frequency")

# creates a bar graph that shows ride is the most freq activity for females
ggplot(data = female.data) +
  geom_bar(mapping = aes(x = type, fill = type), show.legend = FALSE) +
  labs(title = "Frequency of Activity for Females", x = "Activity", y = "Count")

# creates a bar graph that shows ride is the most freq activity for males
ggplot(data = male.data) +
  geom_bar(mapping = aes(x = type, fill = type), show.legend = FALSE) +
  labs(title = "Frequency of Activity for Males", x = "Activity", y = "Count")

# maps the countries with athletes and their frequencies
mapDevice("x11")
mapCountryData(locmap, nameColumnToPlot = "Frequencies", catMethod = "quantiles", missingCountryCol = gray(.9))

# multiple regression to see the relationships between variables
lm(average_speed~athlete.sex+distance, data = focus.data)

lm(average_speed~athlete.sex+type, data = focus.data)

# linear regression to see the relationships between variables
lm(average_speed~athlete.sex, data = focus.data)

lm(distance~athlete.sex, data = focus.data)

lm(elapsed_time~athlete.sex, data = focus.data)


