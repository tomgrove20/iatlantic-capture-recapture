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
library(gridExtra)        # Required for graph panel


# STEP 1: Upload dataset
id_cleaned <- read.csv(file="intermediate-products/hb_matches_bermuda_cleaned.csv", header=TRUE, sep=",")

head(id_cleaned) # Just to take a look at the dataset
summary(id_cleaned) # we can see that there are two rows with na in date.

id_cleaned <- id_cleaned %>%
  drop_na(date) %>%
  mutate(year = lubridate::year(date)) %>% # add an extra column for year alone
  mutate(month = lubridate::month(date)) # add extra column for month alone
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

ggplot(data=whales_per_year, aes(year, n)) + geom_bar(stat="identity") + xlab("Year") + ylab("Number of identified whales")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank()  )

ggsave("intermediate-products/hbs_per_year.png")


# E) Within-season distribution total

days_per_month <- id_cleaned %>%
  count(date, month) %>%
  count(month)
days_per_month$month <- month.abb[days_per_month$month]

ggplot(data=days_per_month, aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Total sighting effort (days)")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank()  ) + scale_x_discrete(limits = month.abb)

ggsave("intermediate-products/effort-days-per-month.png")

# F) Within-season distribution for each season

# Starting with days

days_eachseason <- id_cleaned %>%
    count(date, month, year) %>%
    count(month, year) 
days_eachseason$month <- month.abb[days_eachseason$month]

# The simple way to do it (kept active):

days_allmonths <- ggplot(days_eachseason, aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("days days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank(), plot.title = element_text(hjust = 0.5)) + scale_x_discrete(limits = month.abb)

days_allmonths + facet_wrap(~year) + theme(strip.text.x = element_text(size = 15))

# I exported this manually so I could decide sizes

# The more complex way (kept closed)- see appendix 1

# And now whales

whales_eachseason <- id_cleaned %>%
  count(month, year)
whales_eachseason$month <- month.abb[whales_eachseason$month]

# The simple way to do it (kept active):

whales_eachseason_plot <- ggplot(whales_eachseason, aes(month, n)) + geom_bar(stat="identity") + xlab("Month") + ylab("Number of identified whales")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank(), plot.title = element_text(hjust = 0.5)) + scale_x_discrete(limits = month.abb)

whales_eachseason_plot + facet_wrap(~year) + theme(strip.text.x = element_text(size = 15))

# Again exporting manually

# Time between first and last day for each year
# We could go to the hassle of convering column 2 (date) from factor to date (appendix 2), but we can just use yearday

season_length <- aggregate(. ~ year, data=id_cleaned, FUN = function(i)max(i) - min(i) + 1)

season_length <- season_length %>%
    select(year, yearday)
season_length

### APPENDICES
## Appendix 1
# days_2005 <- ggplot(subset(days_eachseason, year %in% 2005), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank() , plot.title = element_text(hjust = 0.5) ) + scale_x_discrete(limits = month.abb) + ylim(0,20) +ggtitle("2005")

# days_2006 <- ggplot(subset(days_eachseason, year %in% 2006), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank() , plot.title = element_text(hjust = 0.5) ) + scale_x_discrete(limits = month.abb) + ylim(0,20) +ggtitle("2006")

# days_2007 <- ggplot(subset(days_eachseason, year %in% 2007), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank() , plot.title = element_text(hjust = 0.5) ) + scale_x_discrete(limits = month.abb)+ ylim(0,20) +ggtitle("2007")

# days_2008 <- ggplot(subset(days_eachseason, year %in% 2008), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank() , plot.title = element_text(hjust = 0.5) ) + scale_x_discrete(limits = month.abb)+ ylim(0,20) +ggtitle("2008")

# days_2009 <- ggplot(subset(days_eachseason, year %in% 2009), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank(), plot.title = element_text(hjust = 0.5)  ) + scale_x_discrete(limits = month.abb) + ylim(0,20) +ggtitle("2009")

# days_2010 <- ggplot(subset(days_eachseason, year %in% 2010), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank() , plot.title = element_text(hjust = 0.5) ) + scale_x_discrete(limits = month.abb) + ylim(0,20) +ggtitle("2010")

# days_2011 <- ggplot(subset(days_eachseason, year %in% 2011), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank() , plot.title = element_text(hjust = 0.5) ) + scale_x_discrete(limits = month.abb) + ylim(0,20) +ggtitle("2011")

# days_2012 <- ggplot(subset(days_eachseason, year %in% 2012), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank()  , plot.title = element_text(hjust = 0.5)) + scale_x_discrete(limits = month.abb) + ylim(0,20) +ggtitle("2012")

# days_2013 <- ggplot(subset(days_eachseason, year %in% 2013), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank() , plot.title = element_text(hjust = 0.5) ) + scale_x_discrete(limits = month.abb) + ylim(0,20) +ggtitle("2013")

# days_2014 <- ggplot(subset(days_eachseason, year %in% 2014), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank() , plot.title = element_text(hjust = 0.5) ) + scale_x_discrete(limits = month.abb) + ylim(0,20) +ggtitle("2014")

# days_2015 <- ggplot(subset(days_eachseason, year %in% 2015), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank(), plot.title = element_text(hjust = 0.5)  ) + scale_x_discrete(limits = month.abb) + ylim(0,20) +ggtitle("2015")

# days_2016 <- ggplot(subset(days_eachseason, year %in% 2016), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank(), plot.title = element_text(hjust = 0.5) ) + scale_x_discrete(limits = month.abb) + ggtitle("2016") + ylim(0,20) + ggtitle("2016")

# days_2017 <- ggplot(subset(days_eachseason, year %in% 2017), aes(month, nn)) + geom_bar(stat="identity") + xlab("Month") + ylab("Sightings days")+ theme_classic(base_family = "helvetica") + theme(axis.title.y = element_text(margin = margin(t = 5, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), axis.ticks.x = element_blank(), plot.title = element_text(hjust = 0.5)) + scale_x_discrete(limits = month.abb) + ggtitle("2017") + ylim(0,20)

# days_season_grid <- grid.arrange(days_2005, days_2006, days_2007, days_2008, days_2009, days_2010, days_2011, days_2012, days_2013, days_2014, days_2015, days_2016, days_2017, nrow=4)

## Appendix 2
#id_cleaned$date <- as.Date(id_cleaned$date, format = "%Y-%m-%d")

#with(id_cleaned, difftime(max(date), min(date)))

