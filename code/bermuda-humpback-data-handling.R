#####################################
### Exploring the ID dataset! #######
#########Tom Grove###################

# Load packages 
library(readr)            # Required for loading data
library(dplyr)            # Required for manipulating data
library(ggplot2)          # Required for creating pretty plots
library(stringr)
library(tidyr)

# STEP 1: Upload dataset
id <- read.csv(file="data/matches_humpback_bermuda_cleaned.csv", header=TRUE, sep=",")
head(id) # Just to take a look at the dataset
 
# STEP 2: get rid of extra columns and give appropriate column names

id <- id %>%
  select(BDA.ID, Day_Month_Year, day.of.year, Serial.Date) %>%
  rename(id = BDA.ID, date = Day_Month_Year, yearday = day.of.year, serialday = Serial.Date)
head(id)

# STEP 3: convert 'dates' to atual dates!

id$date <- gsub('_', '.', id$date)
id$date <- as.Date.character(id$date, format = "%d.%m.%Y")
head(id)


