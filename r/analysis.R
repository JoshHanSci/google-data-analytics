###############################################################################

# Analysis

# Library Setup
install.packages("chron")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("zoo")

library(lubridate)
library(tidyverse)
library(data.table)
library(zoo)


# Setting workspace directory
setwd("your/working/directory")

# Creating data frames
td      <- read.csv("td_cleaned.csv")
members <- subset(td, member_casual=="member")
casual  <- subset(td, member_casual=="casual")

###############################################################################

# Percentage of Total Rides

# Create data frame
casual_percent  <- round(nrow(casual) / nrow(td), 2)
member_percent  <- round(nrow(members) / nrow(td), 2)

Percentage      <- c(casual_percent, member_percent)

percent_total_ride <- data.frame(Count, Percentage)
percent_total_ride <- rename(percent_total_ride, percent = Percentage)
percent_total_ride

# Plot percentage of total rides
ggplot(percent_total_ride) +
  geom_bar(aes(x="", y=percent, fill=member_casual), 
           stat="identity", 
           width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(x=1, y=cumsum(percent)-percent/2.3, 
                label=c(member_percent, casual_percent))) +
  labs(fill="Type of Riders") +
  theme_void()

###############################################################################

# Time on Bike Share

# First plot (under 3 hours)
ggplot(td) + 
  geom_histogram(data=subset(td, time_spent<=10800),   # 3 Hours
                 aes(x=time_spent, 
                     fill=factor(member_casual)),
                 binwidth=60) +
  labs(title="# of Casual/Members Bike Usage over Time Spent (0-3 hours)",
       subtitle="Seconds grouped in 60 second intervals",
       fill="Type of Rider") +
  theme(plot.title=element_text(hjust=0.5), 
        plot.subtitle=element_text(hjust=0.5)) +
  xlab("Time Spent (Seconds)") +
  ylab("# of Rides") + 
  facet_wrap(~member_casual) 

# Second plot (over 3 hours)
ggplot(td) + 
  geom_histogram(data=subset(td, time_spent>10800),
                 aes(x=time_spent,
                     fill=factor(member_casual)),
                 binwidth=60) +
  labs(title="# of Casual/Members Bike Usage over Time Spent (over 3 hours)",
       subtitle="Seconds grouped in 60 second intervals",
       fill="Type of Rider") +
  theme(plot.title=element_text(hjust=0.5), 
        plot.subtitle=element_text(hjust=0.5)) +
  xlab("Time Spent (Seconds)") +
  ylab("# of Rides") + 
  coord_cartesian(xlim=c(10800, 500000) ,ylim=c(0, 260)) +
  facet_wrap(~member_casual)

###############################################################################

# Type of Bike Used

# Plot type of bike used (count)
ggplot(td, aes(x=rideable_type, y=..count.., 
               group=member_casual, fill=member_casual)) +
  geom_bar() +
  theme(plot.title=element_text(hjust=0.5)) +
  facet_wrap(~member_casual) +
  labs(title="# of Bike Type Usage",
       fill="Rider Type") +
  xlab("Rideable Bike Type") +
  ylab("Count")

# Plot type of bike used (proportion)
ggplot(td) +
  geom_bar(aes(x=rideable_type, y=..prop.., 
               group=member_casual, fill=member_casual)) +
  theme(plot.title=element_text(hjust=0.5)) +
  facet_wrap(~member_casual) +
  labs(title="# of Bike Type Usage",
       fill="Rider Type") +
  xlab("Rideable Bike Type") +
  ylab("Proportion")

###############################################################################

# Daily Usage

# Create data frame
hourly_analysis <- td %>%
  select(-(date_started_at:end_lng), -(day_started_at:time_spent), -(X:rideable_type)) %>%
  mutate(Hour=format(as.POSIXct(started_at, format="%Y-%m-%d %H:%M"), format="%H"))

View(hourly_analysis)
# Plot daily usage
ggplot(hourly_analysis) +
  geom_bar(aes(x=Hour, y=..count..,
               group=member_casual,
               fill=member_casual)) +
  theme(plot.title=element_text(hjust=0.5), 
        axis.text.x=element_text(angle=45)) +
  facet_wrap(~member_casual, ) +
  labs(title="# of Casual/Members Bike Usage per Hour",
       fill="Type of Riders") +
  xlab("Hour (24 Hour Time)") +
  ylab("# of Rides")

###############################################################################

# Weekly Usage

# Create a hierarchy based on day_of_the_week
days_of_the_week   <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
                        "Friday", "Saturday", "Sunday")
td$day_started_at  <- factor(td$day_started_at,  
                                 levels = days_of_the_week)

# Plot weekly usage
ggplot(td) +
  geom_bar(aes(x=day_started_at, y=..count.., 
               group=member_casual, 
               fill=member_casual)) +
  theme(plot.title=element_text(hjust=0.5), 
        axis.text.x=element_text(angle=45)) +
  facet_wrap(~member_casual) +
  labs(title="# of Casual/Members Bike Usage per Day of the Week",
       fill="Type of Riders") +
  xlab("Day of the Week") +
  ylab("# of Rides")

###############################################################################

# Seasonal Usage

#Create data frames
member_seasonal <- members %>% 
  mutate(Time=as.factor(as.yearmon(date_started_at))) %>%
  group_by(Time) %>%
  summarise(`# of Rides`=n())
member_seasonal$`Type of Rider` <- "members"

casual_seasonal <- casual %>% 
  mutate(Time=as.factor(as.yearmon(date_started_at))) %>%
  group_by(Time) %>%
  summarise(`# of Rides`=n())
casual_seasonal$`Type of Rider` <- "casual"

seasonal <- rbind(casual_seasonal, member_seasonal)

# Plots seasonal usage
ggplot(seasonal, aes(Time, `# of Rides`, fill=`Type of Rider`)) +
  geom_bar(position="stack", stat="identity") +
  theme(plot.title=element_text(hjust=0.5), 
        axis.text.x=element_text(angle=45)) +
  labs(title="# of Casual/Members Bike Usage over Time")

###############################################################################

# Location

# Set up data frames
start_casual <- count(casual, start_station_name)
start_casual <- start_casual[order(start_casual$n, decreasing=TRUE),]
start_casual <- start_casual[1:10,]

end_casual   <- count(casual, end_station_name)
end_casual   <- end_casual[order(end_casual$n, decreasing=TRUE),]
end_casual   <- end_casual[1:10,]

start_member <- count(members, start_station_name)
start_member <- start_member[order(start_member$n, decreasing=TRUE),]
start_member <- start_member[1:10,]

end_member <- count(members, end_station_name)
end_member <- end_member[order(end_member$n, decreasing=TRUE),]
end_member <- end_member[1:10,]

# Print data frames
start_casual[]
end_casual[]
start_member[]
end_member[]