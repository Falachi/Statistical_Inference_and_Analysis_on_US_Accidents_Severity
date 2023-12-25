library(tidyverse)
library(caret)
library(caTools)
library(class)
#importing the dataset
dataset <- read_csv('reduced_data.csv')

#Checking the properties of the dataset
str(dataset) #information of the features data types
summary(dataset) #descriptive statistic of the dataset

#checking the null values of each column
null_counts <- colSums(is.na(dataset))
print(null_counts)

# sum of columns
ncol(dataset)
# sum of null values
sum(null_counts)
#exracting the time values from the date
#first is to change the time to one timezone only. using the mode to replace null values in the timezone column
mode_timezone <- as.character(names(sort(table(dataset$Timezone), decreasing = TRUE)[1]))
dataset$Timezone[is.na(dataset$Timezone)] <- mode_timezone
unique(dataset$Timezone)

library(lubridate)
# Create a mapping of categorical values to time zones
timezone_mapping <- c("US/Pacific" = "America/Los_Angeles", "US/Eastern" = "America/New_York", "US/Central" = "America/Chicago", "US/Mountain" = "America/Denver")

# Use 'force_tz' to adjust the timezones of the 'Start_Time' column based on the 'Timezone' column
dataset$Start_Time <- force_tz(dataset$Start_Time, tzone = timezone_mapping[dataset$Timezone])
dataset$Start_Time[1:5]

#Changing the date and time into specified category otherwise referred to as time-of-day binning
dataset$Time_Of_Day <- cut(hour(dataset$Start_Time), breaks = c(0, 6, 12, 18, 24), 
                         labels = c("Night", "Morning", "Afternoon", "Evening"),
                         include.lowest = TRUE)
dataset$Time_Of_Day[1:5]

#dropping redundant columns
reduced_data <- dataset %>%
  select(c(3, 21:ncol(dataset)))
str(reduced_data)

#checking remaining null values
colSums(is.na(reduced_data))

#important columns to handle null values are precipitation and wind chill. This is because
#they are missing the majority of their values (around 1/4th). So even if we want to drop it,
#it would results in 1/4 of the data gone.

#replacing percipitation null values to 0
reduced_data$`Precipitation(in)`[is.na(reduced_data$`Precipitation(in)`)] <- 0
summary(reduced_data$`Precipitation(in)`)

#wind chill can be calculated using a formula
calculateWindChill <- function(temperature, windSpeed) {
  windChill <- 35.74 + 0.6215 * temperature - 35.75 * windSpeed ^ 0.16 + 0.4275 * temperature * windSpeed ^ 0.16
  return(windChill)
}

#requires to impute temperature and wind speed to ensure all wind chill values can be calculated.
#using the mean based on the time and month is useful for an accurate temperature imputation

#the month can be added from the original data
reduced_data$Month <- format(dataset$Start_Time, "%m")
unique(reduced_data$Month)

#the temperature can now be imputed by the mean based on the day of time and month
mean_temperature_data <- reduced_data %>%
  group_by(Month, Time_Of_Day) %>%
  summarize(Mean_Temperature = mean(`Temperature(F)`, na.rm = TRUE))

reduced_data <- reduced_data %>%
  left_join(mean_temperature_data, by = c("Month", "Time_Of_Day")) %>%
  mutate(`Temperature(F)` = ifelse(is.na(`Temperature(F)`), Mean_Temperature, `Temperature(F)`)) %>%
  select(-Mean_Temperature)

#the wind speed, unfortunately, requires on an already established weather model, or reading
#from instruments. It will instead be imputed through either the mean or mode for analytical
#and modeling purposes. It's first visualize to find whether the mode or mean is better.

# Create a histogram
hist(reduced_data$`Wind_Speed(mph)`, breaks = 20, main = "Wind Speed Distribution", xlab = "Wind Speed (mph)")

# Create a density plot
density_plot <- density(reduced_data$`Wind_Speed(mph)`, na.rm = TRUE)
plot(density_plot, main = "Wind Speed Density Plot", xlab = "Wind Speed (mph)")

#replace null values with the median
reduced_data$`Wind_Speed(mph)` <- ifelse(is.na(reduced_data$`Wind_Speed(mph)`), 
                                         median(reduced_data$`Wind_Speed(mph)`, na.rm = TRUE),
                                         reduced_data$`Wind_Speed(mph)`)

#now, the wind chill can be calculated with the complete tempearture and wind speed variable.
reduced_data <- reduced_data %>% mutate(`Wind_Chill(F)` = ifelse(!is.na(`Temperature(F)`) & !
                                                                   is.na(`Wind_Speed(mph)`),
                                                                 calculateWindChill(`Temperature(F)`, `Wind_Speed(mph)`), NA))

#checking whether the median or mean is better suited for density %
#creating a density plot based on the month
library(ggplot2)
gg <- ggplot(reduced_data, aes(x = `Humidity(%)`, fill = factor(Month)))
gg + geom_density(alpha = 0.25) + labs(title = "Density Plot of Humidity % by Month")

#replace null values with the median
reduced_data$`Humidity(%)` <- ifelse(is.na(reduced_data$`Humidity(%)`), 
                                         median(reduced_data$`Humidity(%)`, na.rm = TRUE),
                                     reduced_data$`Humidity(%)`)

#imputing the missing pressure values based on temperature and precipitation
plot(reduced_data$`Temperature(F)`, reduced_data$`Precipitation(in)`, 
     xlab = "Temperature (F)", ylab = "Precipitation (in)",
     main = "Scatterplot of Temperature vs. Precipitation")

#linear regression won't work, therefore, we can use KNN
variables_to_impute <- "Pressure(in)"
dist_variables <- c("Temperature(F)", "Precipitation(in)")

reduced_data <- knn(
  data = reduced_data,
  variable = variables_to_impute,
  dist_var = dist_variables,
  k = 5,  # You can adjust the number of nearest neighbors as needed
  impNA = TRUE
)

#using precipitation, pressure and temperature to impute weather condition through KNN
variables_to_impute <- "Weather_Condition"
dist_variables <- c("Temperature(F)", "Precipitation(in)", "Pressure(in)")

#visibility can be imputed through finding the mean based on the weather condition
mean_visibility_data <- reduced_data %>%
  group_by(Weather_Condition) %>%
  summarize(Mean_Visibility = mean(`Visibility(mi)`, na.rm = TRUE))

reduced_data <- knn(
  data = reduced_data,
  variable = variables_to_impute,
  dist_var = dist_variables,
  k = 5,  # You can adjust the number of nearest neighbors as needed
  impNA = TRUE
)
reduced_data <- reduced_data %>%
  left_join(mean_visibility_data, by = c("Weather_Condition")) %>%
  mutate(`Visibility(mi)` = ifelse(is.na(`Visibility(mi)`), Mean_Visibility, `Visibility(mi)`)) %>%
  select(-Mean_Visibility)

#wind direction can be imputed with the mode based on weather_condition for accuracy
# Custom mode function
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Impute Wind_Direction grouped by Weather_Condition using mode
reduced_data <- reduced_data %>% 
  group_by(Weather_Condition) %>%
  mutate(Wind_Direction = ifelse(is.na(Wind_Direction), mode(Wind_Direction), Wind_Direction)) %>%
  ungroup()

#sunrise sunset and the 3 twilight variables can be removed as they are similar to time of day
#variable while providing less
reduced_data <- subset(reduced_data, select = -c(Sunrise_Sunset, Civil_Twilight, Nautical_Twilight, Astronomical_Twilight))

#removing variables used solely for pre-processing
reduced_data <- subset(reduced_data, select = -c(`Pressure(in)_imp`, `Weather_Condition_imp`))

#changing the target value to character data type
reduced_data$Severity <- as.character(reduced_data$Severity)

#checking the properties of the final prepossessed data
str(reduced_data)
summary(reduced_data)
colSums(is.na(reduced_data))

#export the data
write.csv(reduced_data, "preprocessed_data.csv", row.names = FALSE)