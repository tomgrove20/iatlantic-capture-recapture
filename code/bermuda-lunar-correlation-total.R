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
library(rcompanion)
library(car)

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

# STEP 3: group by date, to produce number of whales seen per day.
lunar_dates <- lunar_id %>%
  count(date)

head(lunar_dates) 


# STEP 3: add extra column for lunar progression and illumination

lunar_dates <- lunar_dates %>%
  mutate(lunar_illumination = lunar.illumination(as.Date(lunar_dates$date))) %>%
  mutate(lunar_phase = lunar.phase(as.Date(lunar_dates$date))) %>%
  drop_na(date)
head(lunar_dates)

# GOOD UP TO HERE

# STEP 4: visualise data

# Let's try scatterplot with simple lm
ggplot(data=lunar_dates, aes(lunar_dates$lunar_illumination, lunar_dates$n)) + geom_point() + xlab("Lunar illumination") + ylab("Number of humpbacks identified per day") + stat_smooth(method = "lm")
# No relationship there!


# It looks like there is perhaps a quadratic relationship. Let's take a look.
ggplot(data=lunar_dates,aes(lunar_dates$lunar_illumination, lunar_dates$n)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2))


# Pretty sure there is no correlation

# EXTRA: is n normally distributed?

# Density plot
ggplot(data=lunar_dates, aes(lunar_dates$n)) + geom_density(kernel = "gaussian") + xlab("Number of humpbacks identified per day") + ylab("Density")

# QQ plot
qqnorm(lunar_dates$n)
qqline(lunar_dates$n,
       col="red")

# Definitely not normal!Let' try to transform!

# Sqrt- nope!
n_sqrt = sqrt(lunar_dates$n)
plotNormalHistogram(n_sqrt)

#Log- not really!
n_log = log(lunar_dates$n)
plotNormalHistogram(n_log)
qqnorm(n_log)
qqline(n_log,
       col="red")

#Tukey's power of ladders
n_tuk = transformTukey(lunar_dates$n,plotit=FALSE)
plotNormalHistogram(n_tuk)
qqnorm(n_tuk)
qqline(n_tuk,
       col="red")

#Boxcox
bc <- boxcox(lunar_dates$n ~ lunar_dates$lunar_illumination)
lambda <- bc$x[which.max(bc$y)]
lambda  
# lambda between 2 and -2, so we can continue

powerTransform <- function(lunar_dates$n, lambda1, lambda2 = NULL, method = "boxcox") {
  
  boxcoxTrans <- function(lunar_dates$lunar_illumination, lam1, lam2 = NULL) {
    
    # if we set lambda2 to zero, it becomes the one parameter transformation
    lam2 <- ifelse(is.null(lam2), 0, lam2)
    
    if (lam1 == 0L) {
      log(lunar_dates$n + lam2)
    } else {
      (((lunar_dates$n + lam2)^lam1) - 1) / lam1
    }
  }
  
  switch(method
         , boxcox = boxcoxTrans(lunar_dates$n, lambda1, lambda2)
         , tukey = lunar_dates$n^lambda1
  )
}

mnew <- lm(powerTransform(lunar_dates$n, lambda) ~ lunar_dates$lunar_illumination)

op <- par(pty = "s", mfrow = c(1, 2))
qqnorm(m$residuals); qqline(m$residuals)
qqnorm(mnew$residuals); qqline(mnew$residuals)
par(op)
# Boxcox doesn't work either! So, we can't get these data to be normally distributed

# EXTRA: play around with plots

ggplot(data=lunar_id, aes(lunar_id$lunar_illumination)) + geom_histogram() + xlab("lunar illumination") 

ggplot(data=lunar_id, aes(lunar_id$lunar_phase)) + geom_histogram() + xlab("lunar phase") 

ggplot(data=lunar_id, aes(lunar_id$lunar_phase)) + geom_histogram() + coord_polar()+ xlab("Lunar phase")
  


# Plots: does the number of humpbacks sighted per day correlate with lunar illumination?


# This step assumes that the  
lunar_days <- lunar_id %>%
  count(date)

head(lunar_days) 

ggplot(data=lunar_days, aes(lunar_days$lunar_illumination)) + geom_histogram() + xlab("lunar illumination") + ylab("number of humpbacks per day")

ggplot(data=lunar_days, aes(lunar_days$lunar_illumination)) + geom_area(stat="bin") + xlab("lunar illumination") + ylab("number of humpbacks per day")

ggplot(data=lunar_days, aes(lunar_days$lunar_illumination, lunar_days$n)) + geom_point() + xlab("lunar illumination") + ylab("Number of humpbacks identified per day")


arrange(lunar_id, date) #ensures that first sighting kept upon removing duplicated
lunar_id_first <- lunar_id[!duplicated(lunar_id$id),]
head(lunar_id_first)
