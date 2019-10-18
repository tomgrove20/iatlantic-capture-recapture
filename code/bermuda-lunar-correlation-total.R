###############################################
### How does the frequency of new sightings ###
### correlate with lunar cycle progression? ###
###############################################
################Tom Grove######################
###############################################

# Load packages 
library(readr)            # Required for loading data
library(dplyr)            # Required for manipulating data
library(tidyr)
library(ggplot2)          # Required for creating pretty plots
library(lunar)            # Require to convert daty to lunar cycle
library(MASS)             # Determine how distribution is
library(OneR)


# STEP 1: Upload dataset, select relevant rows and rename
lunar_id <- read.csv(file="data/hb_ids_bermuda_incduplicates.csv", header=TRUE, sep=",")

lunar_id <- lunar_id %>%
  dplyr::select(BDA.ID, Day_Month_Year, Day.of.Year, Serial.Date) %>%
  rename(id = BDA.ID, date = Day_Month_Year, yearday = Day.of.Year, serialday = Serial.Date)
head(lunar_id) # Just to take a look at the dataset


# STEP 2: convert 'dates' to atual dates!

lunar_id$date <- gsub('_', '.', lunar_id$date)
lunar_id$date <- as.Date.character(lunar_id$date, format = "%d.%m.%Y")
head(lunar_id)


# STEP 3: add extra column for lunar progression and illumination

lunar_id <- lunar_id %>%
  mutate(lunar_illumination = lunar.illumination(as.Date(lunar_id$date))) %>%
  mutate(lunar_phase = lunar.phase(as.Date(lunar_id$date))) %>%
  drop_na(date)
head(lunar_id)

## GOOD TO HERE

# STEP 4: now need to group by date sighted (ie number of humpbacks per day sighted)

ggplot(data=lunar_id, aes(lunar_id$lunar_illumination)) + geom_histogram() + xlab("lunar illumination") 

ggplot(data=lunar_id, aes(lunar_id$lunar_phase)) + geom_histogram() + xlab("lunar phase") 

ggplot(data=lunar_id, aes(lunar_id$lunar_phase)) + geom_histogram() + coord_polar()+ xlab("Lunar phase")
  

lunar_test <- lunar_id %>%
  count(lunar_illumination)

head(lunar_test)  


arrange(lunar_id, date) #ensures that first sighting kept upon removing duplicated
lunar_id_first <- lunar_id[!duplicated(lunar_id$id),]
head(lunar_id_first)

# STEP 4: save as .csv

write.csv(lunar_id, file="intermediate-products/hb_matches_bermuda_lunar.csv")



# STEP 4: histogram of lunar cycle


