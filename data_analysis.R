

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(scales)


# Set working directory
setwd("C:/Users/ronti/Desktop/ron_r_project")

#import cleaned data
cleaned_data <- read_csv("cleaned_data/cleaned_data.csv")

summary(cleaned_data)
views(cleaned_data)

#######################################################

# Count of Rideable Types by Membership Type

cleaned_data %>%
  group_by(member_casual) %>%
  count(rideable_type) %>%
  ggplot(aes(x = member_casual, y = n, fill = rideable_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar chart
  geom_text(aes(label = comma(n)),                                              # Format count with commas
            position = position_dodge(width = 0.8), vjust = -0.5, size = 4) +  # Adjust vjust for placement
  labs(title = "Number of Ride by Membership in 2023",
       x = "Membership Status",
       y = "Count",
       fill = "Rideable Type") +
  theme_minimal() +
  theme(text = element_text(size = 10))

#######################################################

# Monthly Ride Trends by Member Types in 2023

cleaned_data %>%
  group_by(member_casual, month) %>%
  count() %>%
  ggplot(aes(x = as.integer(month), y = n, color = member_casual, group = member_casual)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:12) +  # Ensure x-axis shows whole numbers (1-12)
  scale_y_continuous(labels = comma) +  # show whole number
  labs(title = "Monthly Ride Trends by Membership in 2023", x = "Month", y = "Total Rides", color = "Member Type") +
  theme_minimal()

#######################################################

# Daily Average Number of Rides by Member Type in 2023

cleaned_data %>%
  mutate(weekday = wday(started_at, label = TRUE, abbr = FALSE)) %>%           # Extract weekday
  group_by(member_casual, weekday) %>%
  summarise(avg_rides = mean(n()), .groups = 'drop') %>%                       # Calculate average rides per weekday
  ggplot(aes(x = weekday, y = avg_rides, fill = member_casual)) +              # Aesthetic mapping
  geom_bar(stat = "identity", position = "dodge") +                            # Bar chart
  geom_text(aes(label = round(avg_rides, 1)),                                  # Add count on bars
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +  # Adjust vjust for placement
  scale_y_continuous(labels = comma) +                                         # Show whole numbers on y-axis
  labs(title = "Daily Average Number of Rides by Membership in 2023",     # Title
       x = "Weekday",                                                          # X-axis label
       y = "Average Number of Rides",                                          # Y-axis label
       fill = "Member Type") +                                                 # Legend title
  theme_minimal()


#######################################################

# Top 10 Highest usage Stations by Member Type in 2023

cleaned_data %>%
  filter(!is.na(start_station_name)) %>%                            # Remove any null start stations
  group_by(start_station_name, member_casual) %>%                   # Group by start station and member type
  count() %>%                                                       # Count the number of rides for each group
  ungroup() %>%
  group_by(start_station_name) %>%                                  # Group by start station for total calculation
  summarise(total_rides = sum(n), .groups = 'drop') %>%             # Summarize total rides per station
  arrange(desc(total_rides)) %>%                                    # Arrange in descending order of total rides
  slice_head(n = 10) %>%  # Select top 10 stations
  inner_join(cleaned_data, by = "start_station_name") %>%           # Join back to get member type counts
  group_by(member_casual, start_station_name) %>%
  count() %>%                                                       # Count rides per member type for the top stations
  ggplot(aes(x = reorder(start_station_name, n), y = n, fill = member_casual)) +                      # Set up ggplot
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +                  # Create bar chart
  labs(title = "Top 10 Highest usage Stations by Membership in 2023",                                 # Title of the plot
       x = "Start Station Name",                  # X-axis label
       y = "Count",                               # Y-axis label
       fill = "Member Type") +                    # Legend title
  coord_flip() +                                  # Flip coordinates for better readability
  theme_minimal() +                               # Minimal theme for clean appearance
  theme(text = element_text(size = 10))           # Adjust text size






