# Load Packages
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(ggplot2)
library(lubridate)

# Set working directory
setwd("C:/Users/ronti/Desktop/ron_r_project")

# Import Data
jan_2023 <- read.csv("data/202301-divvy-tripdata.csv")
feb_2023 <- read.csv("data/202302-divvy-tripdata.csv")
mar_2023 <- read.csv("data/202303-divvy-tripdata.csv")
apr_2023 <- read.csv("data/202304-divvy-tripdata.csv")
may_2023 <- read.csv("data/202305-divvy-tripdata.csv")
jun_2023 <- read.csv("data/202306-divvy-tripdata.csv")
jul_2023 <- read.csv("data/202307-divvy-tripdata.csv")
aug_2023 <- read.csv("data/202308-divvy-tripdata.csv")
sep_2023 <- read.csv("data/202309-divvy-tripdata.csv")
oct_2023 <- read.csv("data/202310-divvy-tripdata.csv")
nov_2023 <- read.csv("data/202311-divvy-tripdata.csv")
dec_2023 <- read.csv("data/202312-divvy-tripdata.csv")


# 1st Data Checking (check column, types)
summary(jan_2023)
summary(feb_2023)
summary(mar_2023)
summary(apr_2023)
summary(may_2023)
summary(jun_2023)
summary(jul_2023)
summary(aug_2023)
summary(sep_2023)
summary(oct_2023)
summary(nov_2023)
summary(dec_2023)

# Combine Data
raw_data <- rbind(jan_2023,feb_2023,mar_2023,
                       apr_2023,may_2023,jun_2023,
                       jul_2023,aug_2023,sep_2023,
                       oct_2023,nov_2023,dec_2023)

summary(raw_data)

#############################################
# Data Cleaning
#############################################

# Step 1: Checking

# check if missing value exist
colSums(is.na(raw_data))    
anyNA(raw_data)

# check if duplicate exist
has_duplicates <- any(duplicated(raw_data))
has_duplicates

# check if start time > endtime
invalid_start_end_time <- any(raw_data$started_at > raw_data$ended_at)
print(invalid_start_end_time)

# Short summary: 
# 1. have NA value
# 2. no duplicate (skip)
# 3. Create time_diff_minutes column
# 4. Filter rental time error
# 5. Filter extreme rental time
# 6. extract datetime to individual variables
# 7. filter extreme rental time > 6hrs
# 8. Extract datetime to year, month, day, hour, minute

# Note: based on checking, clean the data.

#############################################

# Step 2: Basic clean up (NA, duplicate, delete column)

# pipe to clean up
data_distinct_no_missing <- raw_data %>%
  distinct() %>%                          # Remove duplicate rows
  drop_na() %>%                           # Remove row with missing value
  remove_missing() %>%                    # Remove row with missing value
  remove_empty(which = c("rows", "cols")) # Remove COMPLETELY empty rows or cols


# checking after pipe
colSums(is.na(data_distinct_no_missing))   
summary(data_distinct_no_missing)

#############################################

# Step 3: Create time_diff_minutes column  +  Filter rental time error  +  Filter extreme rental time

cleaned_datetime <- data_distinct_no_missing %>%
  mutate(
    started_at = ymd_hms(started_at),  # Convert to POSIXct if not already in date-time format
    ended_at = ymd_hms(ended_at),      # Convert to POSIXct if not already in date-time format
    time_diff_minutes = as.numeric(difftime(ended_at, started_at, units = "mins")) 
  ) %>%
  filter(started_at <= ended_at & time_diff_minutes > 0 & time_diff_minutes < 360)

# checking after pipe
summary(cleaned_datetime)

# check if start_time > ended_time still exist
checking_start_end_time <- any(cleaned_datetime$started_at > cleaned_datetime$ended_at)
print(checking_start_end_time)

#############################################

# Step 4: Extract (year, month, day, hour, minutes)

extracted_time_data <- cleaned_datetime %>%
  mutate(
    year = year(started_at),           # Extract year
    month = month(started_at),         # Extract month
    day = day(started_at),             # Extract day
    hour = hour(started_at),           # Extract hour
    minute = minute(started_at)        # Extract minute
  ) %>%
  drop_na()
  
# View the updated data frame
summary(extracted_time_data)

#############################################

#Save the cleaned data

cleaned_data <- extracted_time_data

write.csv(cleaned_data,file = "cleaned_data/cleaned_data.csv",row.names = FALSE)

