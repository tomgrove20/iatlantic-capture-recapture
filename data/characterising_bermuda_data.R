#####################################
##### Exploring the ID dataset! #####
############# Tom Grove #############

# AIM: characterise the cleaned Bermudadataset (including within-year duplicates) with some basic summary statistics.

# Load packages 
library(readr)            # Required for loading data
library(dplyr)            # Required for manipulating data
library(ggplot2)          # Required for creating pretty plots
library(tidyr)
library(RColorBrewer)
library(lubridate)


# STEP 1: Upload dataset
id_cleaned <- read.csv(file="intermediate-products/hb_matches_bermuda_cleaned.csv", header=TRUE, sep=",")

head(id_cleaned) # Just to take a look at the dataset
summary(id_cleaned) # we can see that there are two rows with na in date.

id_cleaned <- id_cleaned %>%
  drop_na(date) %>%
  mutate(year = lubridate::year(date)) %>% # add an extra column for year alone
  mutate(month = lubridate::month(date))
summary(id_cleaned)

# STEP 2: Number of sightings. 1 sighting = 1 humpback on 1 day (ie 1 row). Look at number of sightings.

# A) Sightings total

nrow(id_cleaned) #nrows = nsightings

# B) Sightings per hb.

sightings_per_hb <- id_cleaned %>%
  count(id)
summary(sightings_per_hb) # includes mean, median, min and max


# Histogram of number of sightings per hb (inc. within-year duplicates)

windowsFonts(helvetica = windowsFont("Helvetica"))

ggplot(data=sightings_per_hb, aes(sightings_per_hb$n)) + geom_bar() + xlab("Number of sightings per whale") + ylab("Number of identified whales") + theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

ggsave("intermediate-products/sightings_per_hb.png")

# C) Number of years per hb

hb_year <- id_cleaned %>% # hb_year: hb x year
  dplyr::select(id, year) %>%
  unique()
  
years_per_whale <- hb_year %>% # number of years per hb
  count(id)
summary(years_per_whale)

ggplot(data=years_per_whale, aes(years_per_whale$n)) + geom_bar() + xlab("Number of years sighted") + ylab("Number of identified whales") + theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank())  + scale_y_continuous(breaks = seq(0,1000, by = 200)) + scale_x_continuous(breaks = seq(0,9, by = 1))

ggsave("intermediate-products/years_per_hb.png")

# D) Number of whales per year.
 
whales_per_year <- hb_year %>% #number of whales per year
  count(year)
summary(whales_per_year)
write.csv(whales_per_year, file = "intermediate-products/whales_per_year.csv",row.names=FALSE)


# E) Within-season distribution

days_per_month <- id_cleaned %>%
  count(date, month) %>%
  count(month)
days_per_month$month <- month.abb[days_per_month$month]

ggplot(data=days_per_month, aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Total sighting effort (days)")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank()  ) + scale_x_discrete(limits = month.abb)

ggsave("intermediate-products/effort-days-per-month.png")

#GOOD UP TO HERE

# STEP 3: convert 'dates' to atual dates!

id_cleaned$date <- gsub('-', '.', id_cleaned$date)
id_cleaned$date <- as.Date.character(id_cleaned$date, format = "%d.%m.%Y")
head(id_cleaned)

# Now this contains all sightings of identified humpbacks (including within-year duplicates in Bermuda, up to 2018!)

# STEP 4: save to intermediate products.
write.csv(id, file = "intermediate-products/hb_matches_bermuda_cleaned.csv",row.names=FALSE)

