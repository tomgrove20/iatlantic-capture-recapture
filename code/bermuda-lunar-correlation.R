###############################################
### How does the frequency of new sightings ###
### correlate with lunar cycle progression? ###
###############################################
################Tom Grove######################
###############################################

# Load packages 
library(readr)            # Required for loading data
library(dplyr)            # Required for manipulating data
library(ggplot2)          # Required for creating pretty plots
library(lunar)            # Require to convert daty to lunar cycle
library(MASS)             # Determine how distribution is
library(tidyr)


# STEP 1: Upload dataset
lunar_id <- read.csv(file="data/matches_humpback_bermuda.csv", header=TRUE, sep=",")
lunar_id <- lunar_id%>%
  dplyr::select(BDA.ID, Day_Month_Year, day.of.year, Serial.Date) %>%
  rename(id = BDA.ID, date = Day_Month_Year, yearday = day.of.year, serialday = Serial.Date)
head(lunar_id)


# STEP 2: add extra column for lunar progression

lunar_id$date <- gsub('_', '.', lunar_id$date) # Make dates, dates!
lunar_id$date <- as.Date.character(lunar_id$date, format = "%d.%m.%Y")
head(lunar_id)



# STEP 3: only want rows which represent the last sighting of a whale

lunar_id <- lunar_id %>%
  mutate(lunar_illumination = lunar.illumination(as.Date(lunar_id$date))) %>%
  mutate(lunar_phase = lunar.phase(as.Date(lunar_id$date))) %>%
  drop_na(date)
head(lunar_id)

lunar_id <-  arrange(lunar_id, date)
lunar_id <-  arrange(lunar_id, rev(date))

lunar_id_last <- lunar_id[!duplicated(lunar_id$id),]
head(lunar_id_last)

# STEP 4: save as .csv

write.csv(id, file="intermediate-products/hb_matches_bermuda_lunar_last.csv")


# STEP 4: histogram of lunar cycle

ggplot(data=lunar_id_last, aes(lunar_id_last$lunar_phase)) + geom_histogram() + xlab("lunar phase")

ggplot(data=lunar_id_last, aes(lunar_id_last$lunar_illumination)) + geom_histogram() + xlab("lunar illumination")



## GOOD TO HERE

# STEP 5: which distribution best fits these data? Clearly not normally distributed (from plot)

x <- lunar_id_first$lunar_phase



fitdistr(x, "normal")

# STEP 4: look for Pearson correlation between first sighting date and lunar cycle 

# STEP 4: save the new csv

write.csv(id, file="intermediate-products/hb_matches_bermuda_cleaned.csv")


