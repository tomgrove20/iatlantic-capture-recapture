#####################################
### Cleaning the ID dataset! #######
#########Tom Grove###################

# AIM: produce a clean dataset of all sightings of identified humpbacks (including within-year duplicates) around Bermuda, up to 2018!

# Load packages 
library(readr)            # Required for loading data
library(dplyr)            # Required for manipulating data
library(ggplot2)          # Required for creating pretty plots
library(stringr)
library(tidyr)

# STEP 1: Upload dataset
id <- read.csv(file="data/hb_ids_bermuda_incduplicates.csv", header=TRUE, sep=",")
head(id) # Just to take a look at the dataset
 
# STEP 2: get rid of extra columns and give appropriate column names

id <- id %>%
  dplyr::select(BDA.ID, Day_Month_Year, Day.of.Year, Serial.Date) %>%
  rename(id = BDA.ID, date = Day_Month_Year, yearday = Day.of.Year, serialday = Serial.Date) %>%
  drop_na(date)
head(id)

# STEP 3: convert 'dates' to atual dates!

id$date <- gsub('_', '.', id$date)
id$date <- as.Date.character(id$date, format = "%d.%m.%Y")
head(id)



# Now this contains all sightings of identified humpbacks (including within-year duplicates in Bermuda, up to 2018!)

# STEP 4: save to intermediate products.
write.csv(id, file = "intermediate-products/hb_matches_bermuda_cleaned.csv",row.names=FALSE)

