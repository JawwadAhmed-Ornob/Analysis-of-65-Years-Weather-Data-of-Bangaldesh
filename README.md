
## Here I'm downloading all the packages


library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(tidyr)

## Loading Dataset

weather_data <- read.csv("D:/R Programming/bd_weather.csv")


## Exploring the Data

head(weather_data)
str(weather_data)
summary(weather_data)
colnames(weather_data)




## Checking for Missing Values

sum(is.na(weather_data))

## Basic Statistics for Rainfall

rainfall_data <- summary(weather_data$Rainfall)
mean_rainfall <-mean(rainfall_data)
median_rainfall <- median(rainfall_data)
standard_deviation_of_rainfall <- sd(rainfall_data)
total_rainfall <- sum(rainfall_data)

print(mean_rainfall)
print(median_rainfall)
print(standard_deviation_of_rainfall)
print(total_rainfall)


## Convert month numbers to month names


weather_data$Month <- month.name[weather_data$Month]
head(weather_data)




## Here Starts Anaysis

## Average Rainfall by Year

avg_rainfall_by_year <- weather_data %>%
  group_by(YEAR) %>%
  summarize(avg_rainfall = mean(Rainfall, na.rm = TRUE)) %>%
  arrange(desc(avg_rainfall)) 
print(avg_rainfall_by_year)

# Finding out the highest average rainy year
highest_average_rainy_year <- head(avg_rainfall_by_year, n=1)
highest_average_rainy_year

# Finding out the lowest average rainy year
lowest_average_rainy_year <- tail(avg_rainfall_by_year, n=1)
lowest_average_rainy_year


# Displaying Average Rainfall in each Year

ggplot(data = avg_rainfall_by_year, aes(x = YEAR, y = avg_rainfall)) +
  geom_smooth(stat = "identity", fill = "skyblue") +
  labs(title = "Average Rainfall by Year", x = "Year", y = "Average Rainfall (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Average rainfall by month 
avg_rainfall_by_month <- weather_data %>%
  group_by(Month) %>%
  summarize(avg_rainfall = mean(Rainfall, na.rm = TRUE)) %>%
  arrange(desc(avg_rainfall)) 
print(avg_rainfall_by_month)


# Highest Average Rainfall By Month

highest_average_rainy_month <- head(avg_rainfall_by_month, n = 1) 
highest_average_rainy_month

# Lowest Average Rainfall By Month

lowest_average_rainy_month<- tail(avg_rainfall_by_month, n=1)
lowest_average_rainy_month


# Displaying the average rainfall by months

ggplot(avg_rainfall_by_month, aes(x = Month, y = avg_rainfall, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Rainfall by Month", x = "Month", y = "Average Rainfall (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Average Rainfall According to Station Names (Location)

avg_rainfall_by_stations <- weather_data %>% 
  group_by(Station.Names) %>% 
  summarise(avg_rainfall=mean(Rainfall, na.rm =TRUE)) %>% 
  arrange(desc(avg_rainfall))

print(avg_rainfall_by_stations)

# Highest Rainfall by Station
highest_average_rainfall_by_station <- head(avg_rainfall_by_stations, n=1)
highest_average_rainfall_by_station



#Lowest Rainfall By Station
lowest_average_rainfall_by_station <- tail(avg_rainfall_by_stations, n=1)
lowest_average_rainfall_by_station



#Displaying Rainfall by Stations (Locations) in Bar Chart

ggplot(data = avg_rainfall_by_stations) +
  geom_bar(mapping = aes(x = Station.Names, y = avg_rainfall, fill =Station.Names), stat = "identity") +
  labs(title = "Average Rainfall by Station",
       x = "Station Name",
       y = "Average Rainfall (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Displaying Rainfall by Stations (Locations) in Dot Plot

# Create a dot plot of total rainfall by station
ggplot(data = avg_rainfall_by_stations, aes(x = avg_rainfall, y = Station.Names)) +
  geom_point(aes(color = Station.Names), size = 3) +
  labs(title = "Average Rainfall by Station",
       x = "Average Rainfall (mm)",
       y = "Station Name") +
  theme_minimal()



#Displaying Rainfall by Stations (Locations) in Box Plot

ggplot(data = avg_rainfall_by_stations, aes(x = avg_rainfall, y = Station.Names)) +
  geom_boxplot(aes(color = Station.Names), size = 3) +
  labs(title = "Average Rainfall by Station",
       x = "Average Rainfall (mm)",
       y = "Station Name") +
  theme_minimal()


#Plotting Maximum & Minimum Temperature Trends over the Years

#Maximum Temperature

ggplot(weather_data, aes(x = YEAR, y = Max.Temp, fill = YEAR)) +
  geom_smooth() +
  labs(title = "Maximum Temperature Trends Over the Years", x = "Year", y = "Temperature (°C)")

#Minimum Temperature

ggplot(weather_data, aes(x = YEAR, y = Min.Temp, fill = YEAR)) +
  geom_smooth() +
  labs(title = "Minimum Temperature Trends Over the Years", x = "Year", y = "Temperature (°C)")


#Adding Average Temperature Column in Dataset
weather_data$Avg.Temp <- (weather_data$Max.Temp + weather_data$Min.Temp)/2



# Average Temperature by Year
avg_temp_by_year <- weather_data %>% 
  group_by(YEAR) %>% 
  summarise(Avg.Temp= mean(Avg.Temp, na.rm = TRUE)) %>% 
  arrange(desc(Avg.Temp)) 
print(avg_temp_by_year, n=)



# Higest Average Temperature by Year  
highest_Average_Temperature_by_year <- head(avg_temp_by_year, n=1)
print(highest_Average_Temperature_by_year)



# Lowest Average Temperature by Year 
lowest_Average_temperature_by_year <- tail(avg_temp_by_year, n=1)
print(lowest_Average_temperature_by_year)



#Displaying Average Temperature in each Year 


#Line Chart

ggplot(data = avg_temp_by_year) +
  geom_smooth(mapping = aes(x = YEAR, y = Avg.Temp, fill= YEAR), stat = "identity") +
  labs(title = "Average Temperature by Year",
       x = "Year",
       y = "Average Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Average Temperature by Month

avg_temp_by_month <- weather_data %>% 
  group_by(Month) %>% 
  summarise(Avg.Temp= mean(Avg.Temp, na.rm = TRUE)) %>% 
  arrange(desc(Avg.Temp)) 
print(avg_temp_by_month)




#Highest Average Temperature by Month
highest_Average_Temperature_by_month <- head(avg_temp_by_month, n=1)
highest_Average_Temperature_by_month



# Lowest Average Temperature by Month
lowest_Average_temperature_by_month <- tail(avg_temp_by_month, n=1)
lowest_Average_temperature_by_month





#Displaying Average Temperature by Month

ggplot(data = avg_temp_by_month) +
  geom_bar(mapping = aes(x = Month, y = Avg.Temp, fill = Month), stat = "identity") +
  labs(title = "Average Temperature by Month",
       x = "Month",
       y = "Average Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Average Temperature by Stations (Location)


avg_temp_by_station <- weather_data %>% 
  group_by(Station.Names) %>% 
  summarise(Avg.Temp= mean(Avg.Temp, na.rm = TRUE)) %>% 
  arrange(desc(Avg.Temp)) 
print(avg_temp_by_station)




# Highest Average Temperature By Station

highest_Average_Temperature_by_station <- head(avg_temp_by_station, n=1)
print(highest_Average_Temperature_by_station)


#Lowest Average Temperature by Station
lowest_Average_temperature_by_station <- tail(avg_temp_by_station, n=1)
print(lowest_Average_temperature_by_station)


# Dsiplaying Average Temperature by Station

ggplot(data = avg_temp_by_station) +
  geom_bar(mapping = aes(x = Station.Names, y = Avg.Temp, fill = Station.Names), stat = "identity") +
  labs(title = "Average Temperature by Station",
       x = "Station",
       y = "Average Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()



## Average Humidity According by Year


avg_humidity_by_year <- weather_data %>% 
  group_by(YEAR) %>% 
  summarise(avg_humidity= mean(Relative.Humidity, na.rm = TRUE)) %>% 
  arrange(desc(avg_humidity)) 
print(avg_humidity_by_year)



# Highest Average Humidity by Year
highest_average_humidity_by_year <- head(avg_humidity_by_year, n=1)
highest_average_humidity_by_year


# Lowest Average Humidity by Year
lowest_average_humidity_by_year <- tail(avg_humidity_by_year, n=1)
lowest_average_humidity_by_year


# Displaying Average Humidity by Year

ggplot(data = avg_humidity_by_year, aes(x = YEAR, y = avg_humidity)) +
  geom_line(color = "blue") + 
  geom_point(color = "darkblue") +  
  labs(title = "Average Humidity by Year",
       x = "Year",
       y = "Average Humidity (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



## Average Humidity by Month


avg_humidity_by_month <- weather_data %>% 
  group_by(Month) %>% 
  summarise(avg_humidity= mean(Relative.Humidity, na.rm = TRUE)) %>% 
  arrange(desc(avg_humidity)) 
print(avg_humidity_by_month)



# Highest Average Humidity by Month
highest_average_humidity_by_month <- head(avg_humidity_by_month, n=1)
highest_average_humidity_by_month


# Lowest Average Humidity by Year
lowest_average_humidity_by_month <- tail(avg_humidity_by_month, n=1)
lowest_average_humidity_by_month


# Displaying Average Humidity by Month


ggplot(data = avg_humidity_by_month, aes(x = Month, y = avg_humidity, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Humidity by Month",
       x = "Month",
       y = "Average Humidity (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3") 


# Average Humidity by Stations (Location)


avg_humidity_by_station <- weather_data %>% 
  group_by(Station.Names) %>% 
  summarise(avg_humidity= mean(Relative.Humidity, na.rm = TRUE)) %>% 
  arrange(desc(avg_humidity)) 
print(avg_humidity_by_station)



# Highest Average Humidity by Station
highest_average_humidity_by_station <- head(avg_humidity_by_station, n=1)
highest_average_humidity_by_station


# Lowest Average Humidity by Year
lowest_average_humidity_by_station <- tail(avg_humidity_by_station, n=1)
lowest_average_humidity_by_station



# Displaying Average Humidity by Station
sum(is.na(avg_humidity_by_month$Month))
sum(is.na(avg_humidity_by_month$avg_humidity))


ggplot(data = avg_humidity_by_station, aes(x = avg_humidity, y = Station.Names, fill = Station.Names)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Humidity by Station",
       x = "Station",
       y = "Average Humidity (%)") +
  theme_minimal() 


## Wind speed

# Average Wind Speed by Year
avg_windspeed_by_year <- weather_data %>%
  group_by(YEAR) %>%
  summarise(avg_windspeed = mean(Wind.Speed, na.rm = TRUE)) %>%
  arrange(desc(avg_windspeed))
print(avg_windspeed_by_year)


# Highest Average Wind Speed by Year
highest_average_windspeed_by_year <- head(avg_windspeed_by_year, n=1)
highest_average_windspeed_by_year

# Lowest Average Humidity by Year
lowest_average_windspeed_by_year <- tail(avg_windspeed_by_year, n=1)
lowest_average_windspeed_by_year


# Displaying average wiind speed by year


ggplot(data = avg_windspeed_by_year, aes(x = YEAR, y = avg_windspeed, fill = avg_windspeed)) +
  geom_line(stat = "identity") +
  labs(title = "Average wind speed by year",
       x = "Year",
       y = "Average Wind Speed (Km") +
  theme_minimal()


## Average Wind speed by Month

avg_windspeed_by_month <- weather_data %>%
  group_by(Month) %>%
  summarise(avg_windspeed = mean(Wind.Speed, na.rm = TRUE)) %>%
  arrange(desc(avg_windspeed))
print(avg_windspeed_by_month)


# Highest Average Wind Speed by Month
highest_average_windspeed_by_month <- head(avg_windspeed_by_month, n=1)
highest_average_windspeed_by_month

# Lowest Average Humidity by Year
lowest_average_windspeed_by_month <- tail(avg_windspeed_by_month, n=1)
lowest_average_windspeed_by_month



## Determine in which month of 2000 the average wind speed was highest?

avg_windspeed_2000 <- weather_data %>%
  filter(YEAR == 2000) %>%
  group_by(Month) %>%
  summarise(avg_windspeed = mean(Wind.Speed, na.rm = TRUE)) %>%
  arrange(desc(avg_windspeed))

# Identify the month with the highest average windspeed
highest_windspeed_2000 <- avg_windspeed_2000 %>%
  slice(1)  # Get the top row with the highest average windspeed

# Print the result
print(highest_windspeed_2000)


# Displaying Average Wind Speed by Month
ggplot(data = avg_windspeed_by_month, aes(x = Month, y = avg_windspeed, fill = avg_windspeed)) +
  geom_bar(stat = "identity") +
  labs(title = "Average wind speed by Month",
       x = "Month",
       y = "Average Wind Speed (Km") +
  theme_minimal()


# Average Wind Speed by Latitude & Longitude


avg_windspeed_by_location <- weather_data %>%
  group_by(LATITUDE, LONGITUDE) %>%
  summarise(avg_windspeed = mean(Wind.Speed, na.rm = TRUE)) %>%
  arrange(desc(avg_windspeed))
print(avg_windspeed_by_location)


# Highest Average Wind Speed by Latitude & Longitude
highest_average_windspeed_by_location <- head(avg_windspeed_by_location, n=1)
highest_average_windspeed_by_location

# Lowest Average Humidity by Latitude & Longitude
lowest_average_windspeed_by_location <- tail(avg_windspeed_by_location, n=1)
lowest_average_windspeed_by_location


# Displaying Average Wind Speed by Latitude & Longitude

ggplot(data = avg_windspeed_by_location, aes(x = LONGITUDE, y = LATITUDE, color = avg_windspeed)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "blue", high = "red") +  # Color gradient from blue (low windspeed) to red (high windspeed)
  labs(title = "Average Windspeed by Latitude and Longitude",
       x = "Longitude",
       y = "Latitude",
       color = "Average Windspeed (km/h)") +
  theme_minimal()


## Average Temperature by Latitude & Longitude

avg_temp_by_location <- weather_data %>%
  group_by(LATITUDE, LONGITUDE) %>%
  summarise(avg_temp = mean(Avg.Temp, na.rm = TRUE)) %>%
  arrange(desc(avg_temp))
print(avg_temp_by_location)


# Highest Average Temperature by Latitude & Longitude
highest_average_temp_by_location <- head(avg_temp_by_location, n=1)
highest_average_temp_by_location

# Lowest Average Temperature by Latitude & Longitude
lowest_average_temp_by_location <- tail(avg_temp_by_location, n=1)
lowest_average_temp_by_location

## Displaying Average Temperature by Latitude & Longitude
ggplot(data = avg_temp_by_location, aes(x = LONGITUDE, y = LATITUDE, color = avg_temp)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "green", high = "purple") +  # Color gradient from blue (cold) to red (hot)
  labs(title = "Average Temperature by Latitude and Longitude",
       x = "Longitude",
       y = "Latitude",
       color = "Average Temperature (°C)") +
  theme_minimal()


## Average Rainfall by Latitude & Longitude
avg_rainfall_by_location <- weather_data %>%
  group_by(LATITUDE, LONGITUDE) %>%
  summarise(avg_rainfall = mean(Rainfall, na.rm = TRUE)) %>%
  arrange(desc(avg_rainfall))
print(avg_rainfall_by_location)


# Highest Average Rainfall by Latitude & Longitude
highest_average_rainfall_by_location <- head(avg_rainfall_by_location, n=1)
highest_average_rainfall_by_location

# Lowest Average Rainfall by Latitude & Longitude
lowest_average_rainfall_by_location <- tail(avg_rainfall_by_location, n=1)
lowest_average_rainfall_by_location


## Displaying Average Rainfall by Latitude & Longitude

ggplot(data = avg_rainfall_by_location, aes(x = LONGITUDE, y = LATITUDE, color = avg_rainfall)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "pink", high = "black") +  
  labs(title = "Average Rainfall by Latitude and Longitude",
       x = "Longitude",
       y = "Latitude",
       color = "Average Rainfall (mm)") +
  theme_minimal()


## Average Humidity by Latitude & Longitude

avg_humidity_by_location <- weather_data %>%
  group_by(LATITUDE, LONGITUDE) %>%
  summarise(avg_humidity = mean(Relative.Humidity, na.rm = TRUE)) %>%
  arrange(desc(avg_humidity))
print(avg_humidity_by_location)



# Highest Average Humidity by Latitude & Longitude
highest_average_humidity_by_location <- head(avg_humidity_by_location, n=1)
highest_average_humidity_by_location

# Lowest Average Humidity by Latitude & Longitude
lowest_average_humidity_by_location <- tail(avg_humidity_by_location, n=1)
lowest_average_humidity_by_location


## Displaying Average Humidity by Latitude & Longitude


ggplot(data = avg_humidity_by_location, aes(x = LONGITUDE, y = LATITUDE, color = avg_humidity)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "black", high = "red") +  
  labs(title = "Average Humidity by Latitude and Longitude",
       x = "Longitude",
       y = "Latitude",
       color = "Average Humidity (%)") +
  theme_minimal()



## Displaying Station by Latitude & Longitude
ggplot(data = weather_data, aes(x = LONGITUDE, y = LATITUDE, label = Station.Names)) +
  geom_point(color = "blue", size = 3) +
  geom_text(vjust = -1, hjust = 1, size = 3, check_overlap = TRUE) +  # Add station names
  labs(title = "Stations by Latitude and Longitude",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()



