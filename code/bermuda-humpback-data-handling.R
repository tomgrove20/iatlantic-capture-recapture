#####################################
### Cleaning the ID dataset! #######
######## Tom Grove ##################

## AIM 1: produce a clean dataset of all sightings of identified humpbacks (including within-year duplicates) around Bermuda, up to 2018!
## AIM 2: condense this dataset to capture histories (series of 0 and 1)

# Load packages 
library(readr)            # Required for loading data
library(dplyr)            # Required for manipulating data
library(ggplot2)          # Required for creating pretty plots
library(stringr)
library(tidyr)
library(magrittr)
library(lubridate)
library(lattice)
library(reshape2)
library(RColorBrewer)

# AIM 1: produce a clean dataset of all sightings of identified humpbacks (including within-year duplicates) around Bermuda, up to 2018!

# STEP A: Upload dataset
id <- read.csv(file="data/hb_ids_bermuda_incduplicates.csv", header=TRUE, sep=",")
head(id) # Just to take a look at the dataset
 
# STEP B: get rid of extra columns and give appropriate column names

id <- id %>%
  dplyr::select(BDA.ID, Day_Month_Year, Day.of.Year, Serial.Date) %>%
  rename(id = BDA.ID, date = Day_Month_Year, yearday = Day.of.Year, serialday = Serial.Date) 

head(id)

# STEP C: convert 'dates' to atual dates!

id$date <- gsub('_', '.', id$date)
id$date <- as.Date.character(id$date, format = "%d.%m.%Y")

id <- id %>% drop_na(date)
head(id)



# Now this contains all sightings of identified humpbacks (including within-year duplicates in Bermuda, up to 2018!)

# STEP D: save to intermediate products.
write.csv(id, file = "intermediate-products/hb_matches_bermuda_cleaned.csv",row.names=FALSE)

## For some reason id_cleaned isn't produced here any more. Difference from id = removed two obs. from the 1990s

## AIM 2: condense this dataset to capture histories (series of 0 and 1)

# STEP A: we need to assign each date a secondary sampling period. Defined period will be from December to May. 

id_cleaned <- id

id_primaries <- id_cleaned %>%
  mutate(event = 
         ifelse (as_date(date) >= as_date('2004-12-01') & as_date(date) <= as_date('2005-05-31'), 1, 
                 ifelse (as_date(date) >= as_date('2005-12-01') & as_date(date) <= as_date('2006-05-31'), 2, 
                         ifelse (as_date(date) >= as_date('2006-12-01') & as_date(date) <= as_date('2007-05-31'), 3, 
                                 ifelse (as_date(date) >= as_date('2007-12-01') & as_date(date) <= as_date('2008-05-31'), 4, 
                                         ifelse (as_date(date) >= as_date('2008-12-01') & as_date(date) <= as_date('2009-05-31'), 5, 
                                                 ifelse (as_date(date) >= as_date('2009-12-01') & as_date(date) <= as_date('2010-05-31'), 6, 
                                                         ifelse (as_date(date) >= as_date('2010-12-01') & as_date(date) <= as_date('2011-05-31'), 7, 
                                                                 ifelse (as_date(date) >= as_date('2011-12-01') & as_date(date) <= as_date('2012-05-31'), 8, 
                                                                         ifelse (as_date(date) >= as_date('2012-12-01') & as_date(date) <= as_date('2013-05-31'), 9, 
                                                                                 ifelse (as_date(date) >= as_date('2013-12-01') & as_date(date) <= as_date('2014-05-31'), 10, 
                                                                                         ifelse (as_date(date) >= as_date('2014-12-01') & as_date(date) <= as_date('2015-05-31'), 11, 
                                                                                                 ifelse (as_date(date) >= as_date('2015-12-01') & as_date(date) <= as_date('2016-05-31'), 12, 
                                                                                                         ifelse (as_date(date) >= as_date('2016-12-01') & as_date(date) <= as_date('2017-05-31'), 13, 0))))))))))))))

# STEP B: delete rows with primary = 0 (I think two dates from August), add column with detect = 1 and delete all irrelevant columns
id_primaries <- id_primaries %>% 
  mutate(detect = 1) %>%
  select(id, event, detect) %>%
  filter(event!=0)

head(id_primaries)

write.csv(id, file = "intermediate-products/id x year-event x detection (dec-may).csv",row.names=FALSE)

# STEP c: recreate capture histories and save

capture_history <- id_primaries %>%
  # remove duplicates, which may occur when individuals are caught multiple times in an event
  # For example, your event may be a year and an individual may be caught multiple times in a year.
  distinct() %>%
  # spread out data. The fill = 0 adds rows for combinations of id and event where individuals were not observerd
  spread(event, detect, fill = 0) %>% 
  # For every individual....
  group_by(id) %>%
  # Paste together 0's and 1's
  # Unite is similar to paste. Here we are pasting the strings together from the second column (first capture event)
  # to the last capture event ("tail(names(.),1)").
  # we don't want any characters separating 0's and 1's, so we use: sep = ""
  unite("ch", 2:tail(names(.),1), sep = "")

write.csv(capture_history, file = "intermediate-products/id x capture history (dec-may).csv",row.names=FALSE)

# STEP D: condense to unique capture histories, with frequencies.

capture_history_unique <- capture_history %>%
  ungroup(id) %>%
  count(ch)

write.csv(capture_history_unique, file = "intermediate-products/unique capture history x frequency (dec-may).csv",row.names=FALSE)

