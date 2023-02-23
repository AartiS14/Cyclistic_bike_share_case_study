options(repos = c(CRAN = "http://cran.rstudio.com"))
#Install and load the necessary packages 
install.packages("tidyverse") # ggplot2 and dplyr are already installed with tidyverse as they are one of the core packages of tidyverse
install.packages("lubridate")
install.packages("knitr")
install.packages("skimr")
install.packages("janitor")
library(tidyverse)
library(lubridate)
library(knitr)
library(skimr)
library(janitor) 

#Import data 
setwd("~/Desktop/cyclistic")
month_01 <- read_csv("~/Desktop/cyclistic/202202-divvy-tripdata.csv")
month_02 <- read_csv("Desktop/cyclistic/202203-divvy-tripdata.csv")
month_03 <- read_csv("Desktop/cyclistic/202204-divvy-tripdata.csv")
month_04 <- read_csv("Desktop/cyclistic/202205-divvy-tripdata.csv")
month_05 <- read_csv("Desktop/cyclistic/202206-divvy-tripdata.csv")
month_06 <- read_csv("Desktop/cyclistic/202207-divvy-tripdata.csv")
month_07 <- read_csv("Desktop/cyclistic/202208-divvy-tripdata.csv")
month_08 <- read_csv("Desktop/cyclistic/202209-divvy-tripdata.csv")
month_09 <- read_csv("Desktop/cyclistic/202210-divvy-tripdata.csv")
month_10 <- read_csv("Desktop/cyclistic/202211-divvy-tripdata.csv")
month_11 <- read_csv("Desktop/cyclistic/202212-divvy-tripdata.csv")
month_12 <- read_csv("Desktop/cyclistic/202301-divvy-tripdata.csv")

#Check the structure of each month's data set
for(i in list(month_01, month_02, month_03, month_04, month_05, month_06, month_07,month_08,month_09,month_10,month_11,month_12))
print(str(i))

#Combining all data from each month into one large data set
final_data <- bind_rows(month_01, month_02, month_03, month_04, month_05, month_06, month_07, month_08, month_09, month_10, month_11, month_12)

# To make sure all the combined data's data-time column is recognized by R for smoother analysis process. 

final_data$started_at <- as.POSIXct(final_data$started_at, format = "%Y-%m-%d %H:%M:%S")
final_data$ended_at <- as.POSIXct(final_data$ended_at, format = "%Y-%m-%d %H:%M:%S")

#Clean the data 

#Dropping all the NA values in the combined data set.
summary(final_data) #shows the data structure and how many NA values is in the combined data set.
final_data_clean <- na.omit(final_data)
summary(final_data_clean) # shows that all NA values in the combines data set has been removed.

#Temporary removing variables that are not used for this analysis before adding new ones. 
final_data_clean <- final_data_clean %>% 
  select(-c(start_station_name,start_station_id,end_station_name,end_station_id,start_lat,start_lng,end_lat,end_lng)) 

#Filtering out the "docked_bike" from column rideable_type as those are times where bike were parked at a certain station across the city and not riden by a member or non-member (casual) of Cyclistic
final_data_clean <- final_data_clean %>% 
  filter(rideable_type != "docked_bike")

#Extract date,day,day of week, month, and year from started_at column and add as new variables to the data frame.
final_data_clean <- final_data_clean %>%
  mutate(date = as.Date(started_at),
         day = day(started_at),
         day_of_week = weekdays(started_at),
         month = month.name[month(started_at)],
         year = year(started_at))

#Calculating the ride length of each bike which was ride either by a member or a non-member (casual) of Cyclistic and adding a new column
final_data_clean <- final_data_clean %>% 
  mutate(ride_length = difftime(ended_at,started_at,units = "secs"))

#filtering out any negative ride length and ride length of zero secs as either they could a wrong data entry or glitch in the geo-tracked)
final_data_clean <- subset(final_data_clean, ride_length >= 1)

# putting the days of the week in the correct order before starting any descriptive analysis
final_data_clean$day_of_week <- factor(final_data_clean$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# putting the months in the correct order before starting any descriptive analysis. Since the data analysis period is from February 2022 to January 2033. February will be come first and January will be last
final_data_clean$month <- factor(final_data_clean$month, levels = c("February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "January"))

#Calculating the descriptive analysis such as average or mean of the ride length and number of ride while grouping members or non-members(casual) of Cyclistic and rideable_type
final_data_clean_analysis1 <- final_data_clean %>% 
  group_by(member_casual,rideable_type) %>% 
  summarize(average_ride_length = mean(ride_length), number_of_rides = n()) %>% 
  arrange(member_casual,rideable_type) 
  
#Calculating the descriptive analysis such as average or mean of the ride length and number of rides for users by day_of_week while grouping members or non-members(casual) of Cyclistic and day of week
final_data_clean_analysis2 <- final_data_clean %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize (average_ride_length = mean(ride_length), number_of_rides = n()) %>% 
  arrange(member_casual, day_of_week)

#Calculating the descriptive analysis such as average or mean of the ride length and number of rides for users by month while grouping members or non-members(casual) of Cyclistic and month
final_data_clean_analysis3 <- final_data_clean %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n(), average_ride_length = mean(ride_length)) %>% 
  arrange(member_casual, month)

#Calculating the descriptive analysis such as max,minimum, and median of ride length while grouping members or non-members(casual) of Cyclistic. 
final_data_clean_analysis4 <- final_data_clean %>% 
  group_by(member_casual) %>% 
  summarize(max_ride_length = max(ride_length), min_ride_length = min(ride_length), 
            median_ride_length = median(ride_length))%>% 
  arrange(member_casual)

# Data Visualization

ggplot(final_data_clean_analysis1) +
  geom_col(mapping = aes(x = member_casual, y = average_ride_length, fill = rideable_type), position = "dodge") +
  labs(title = "Cyclistic Bike: Average Ride Lenght Vs Type Of Customers", subtitle = "Seprated By The Type Of Bikes Used", 
       caption = "Data obtained from DIVVY website and published by Motivate International Inc")

ggplot(final_data_clean_analysis1) +
  geom_col(mapping = aes(x = member_casual, y = number_of_rides, fill = rideable_type), position = "dodge") +
  labs(title = "Cyclistic Bike: Number Of Rides Vs Type Of Customers", subtitle = "Seprated By The Type Of Bikes Used", 
       caption = "Data obtained from DIVVY website and published by Motivate International Inc")

ggplot(final_data_clean_analysis2) +
  geom_col(mapping = aes(x = day_of_week, y = number_of_rides, fill = member_casual), position = "dodge") +
  labs(title = "Cyclistic Bike: Number Of Rides Vs Day Of Week", subtitle = "Seprated By The Type Of Cyclistic's Customers", 
       caption = "Data obtained from DIVVY website and published by Motivate International Inc")

ggplot(final_data_clean_analysis2) +
  geom_col(mapping = aes(x = day_of_week, y = average_ride_length, fill = member_casual), position = "dodge") +
  labs(title = "Cyclistic Bike: Average Ride Length Vs Day Of Week", subtitle = "Seprated By The Type Of Cyclistic's Customers", 
       caption = "Data obtained from DIVVY website and published by Motivate International Inc")

ggplot(final_data_clean_analysis3) +
  geom_col(mapping = aes(x = month, y = number_of_rides, fill = member_casual), position = "dodge") +
  labs(title = "Cyclistic Bike: Number Of Rides Vs Month", subtitle = "Seprated By The Type Of Cyclistic's Customers", 
       caption = "Data obtained from DIVVY website and published by Motivate International Inc")

ggplot(final_data_clean_analysis3) +
  geom_col(mapping = aes(x = month, y = average_ride_length, fill = member_casual), position = "dodge") +
  labs(title = "Cyclistic Bike: Average Ride Length Vs Month", subtitle = "Seprated By The Type Of Cyclistic's Customers", 
       caption = "Data obtained from DIVVY website and published by Motivate International Inc")