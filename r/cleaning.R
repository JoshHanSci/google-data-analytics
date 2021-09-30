###############################################################################

# Creating New Variables

install.packages("chron")
install.packages("lubridate")
install.packages("tidyverse")

library(chron)
library(lubridate)
library(tidyverse)

setwd("your/working/directory")

###############################################################################

# Merge Several Data Sources

# Mainly doing this because it'll make the end code more legible and there's two different data type formats
tripdata_all_1 <- list.files(path = "path1", 
                           pattern = "*.csv",
                           full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows

tripdata_all_2 <- list.files(path = "path2", 
                           pattern = "*.csv",
                           full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows

# Combine datasets
tripdata_combined <- rbind(tripdata_all_1, tripdata_all_2)

# Export data frame
write.csv(tripdata_combined, "tripdata_combined.csv", row.names = TRUE)

###############################################################################

# Create New Columns

# Create data frames
tripdata_combined <- read.csv("tripdata_combined.csv")
col1 <- c("date_started_at", "time_started_at")
col2 <- c("date_ended_at", "time_ended_at")

tripdata_combined <- tripdata_combined %>%
  separate(
    col = started_at, 
    sep = " ",
    into = col1, 
    remove = FALSE)

# Add date/time columns
tripdata_combined$date_started_at <- ymd(as.character(tripdata_combined$date_started_at))
tripdata_combined$time_started_at <- hms(as.character(tripdata_combined$time_started_at))

tripdata_combined <- tripdata_combined %>%
  separate(
    col = ended_at, 
    sep = " ",
    into = col2, 
    remove = FALSE)

# Add date/time columns
tripdata_combined$date_ended_at <- ymd(as.character(tripdata_combined$date_ended_at))
tripdata_combined$time_ended_at <- hms(as.character(tripdata_combined$time_ended_at))

tripdata_combined$time_spent <- difftime(tripdata_combined$ended_at, tripdata_combined$started_at)

#Adding day of the week column
tripdata_combined$day_started_at <- weekdays(tripdata_combined$date_started_at)
tripdata_combined$day_ended_at   <- weekdays(tripdata_combined$date_ended_at)

#Removes index columns
tripdata_combined <- tripdata_combined %>%
  select(-X)
View(tripdata_combined)

#Export data frame
write.csv(td_combined, "tripdata_formatted.csv", row.names=TRUE)

###############################################################################

# Data Cleaning

# Create data frame
td_formatted <- read.csv("tripdata_formatted.csv")
td_complete_rows <- read.csv("td_complete_rows.csv")

# Removes index column
td_formatted <- td_formatted %>%
  select(-X, -X.1, -...1)

# Removes NA Values
td_complete_rows <- na.omit(td_formatted)

#Check for duplicated entries
td_complete_rows[duplicated(td_complete_rows)]        #No duplicate rows

#Remove Outliers (Geolocation)
min(td_complete_rows$start_lat) #  41.6485
max(td_complete_rows$start_lat) #  42.0649

min(td_complete_rows$end_lat)   #  41.6485
max(td_complete_rows$end_lat)   #  42.07551

min(td_complete_rows$start_lng) # -87.7747
max(td_complete_rows$start_lng) # -87.52823

min(td_complete_rows$end_lng)   # -87.77477
max(td_complete_rows$end_lng)   # -87.52452


# Remove Outliers (Date/Time)
td_complete_rows %>%                    #  April 1st 2020 00:00:30 - June 30th 2021 23:59:59
  summarise(min = min(started_at), 
            max = max(started_at))

td_complete_rows %>%                    #  April 1st 2020 00:10:45 - July 13th 2021 22:51:35
  summarise(min = min(ended_at), 
            max = max(ended_at))

td_complete_rows %>%
  summarise(time_min = min(time_spent) , #  -1,742,998 seconds ~ -20 days 
            time_max = max(time_spent),  #   3,523,202 seconds ~ 40.8 days
            time_avg = mean(time_spent)) #   1,524.847 seconds ~ 25 minutes

# For simplicity for now, we'll just remove problematic (negative) rows
min_time_spent <- min(td_complete_rows$time_spent)

while (min_time_spent <= 0) {
  row = which(td_complete_rows$time_spent == min_time_spent)
  td_complete_rows <- td_complete_rows[-c(row), ]
  min_time_spent <- min(td_complete_rows$time_spent)
}

#Or

new_td_complete_rows <- subset(td_complete_rows, 
                           time_spent > 0)

#Export data frame
td_complete_rows <- td_complete_rows %>%
  select(-X, -X.1)
write.csv(td_complete_rows, "td_cleaned.csv", row.names = TRUE)
