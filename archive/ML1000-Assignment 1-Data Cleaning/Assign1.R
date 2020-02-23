crashes_data <- read.csv('../crashes.csv')

##EXAMINING DATA SET
dim(crashes_data)
head(crashes_data,6)
tail(crashes_data,6)
table(crashes_data$CONTRIBUTING.FACTOR.crashes.1)
summary(crashes_data$CRASH.DATE)
names(crashes_data)
var(crashes_data$CRASH.DATE)
sd(crashes_data$CRASH.DATE)

##HISTOGRAMS

# Random subset of data for ease of computation for testing
partialdata <- crashes_data[sample(1:nrow(crashes_data), 1000,replace=FALSE),];

# Create vectors of just the hours of crashes for data checking
hourOfCrash <- as.numeric(gsub("[:punct:][0-9]{2}$", "", crashes_data$CRASH.TIME));
hourOfCrash_scaled <- scale(hourOfCrash);

# Sanity check of hours of crashes
summary(hourOfCrash);

# Plot the hours of crashes for visual verification
hist(hourOfCrash_scaled);
hist(hourOfCrash);

# Create vectors of just the months of crashes for data checking
monthOfCrash <- as.numeric(gsub("[:punct:][0-9]{2}[:punct:][0-9]{4}$", "", crashes_data$CRASH.DATE));
monthOfCrash_scaled <- scale(monthOfCrash);

# Sanity check of month of crashes
summary(monthOfCrash);

# Plot the month of crashes for visual verification
hist(monthOfCrash);
hist(monthOfCrash_scaled);

##DROP YEAR FROM DATE

crash_dates <- as.Date(crashes_data$CRASH.DATE, "%m/%d/%Y")
crash_dates_no_year <- format(crash_dates, format="%m/%d")
crash_dates_no_year <- as.Date(crashes_data$CRASH.DATE, "%m/%d")
summary(crash_dates_no_year)

table(crash_dates_no_year)
hist(crash_dates_no_year, breaks=366, format = "%d %b", freq=TRUE)

barplot(table(crash_dates_no_year))

library("ggpubr")
crash_dates_no_year_frame <- as.data.frame(crash_dates_no_year)
ggdensity(crash_dates_no_year_frame, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")
library(ggpubr)
ggqqplot(crash_dates_no_year_frame)

crash_dates_no_year_numeric <- as.numeric(crash_dates_no_year)
table(crash_dates_no_year_numeric)


sampleddata <- sample(crash_dates_no_year_numeric,5000,replace=TRUE)

shapiro.test(sampleddata)

##normality test for time

crash_time_numeric<- as.numeric(crashes_data$CRASH.TIME)
table(crash_time_numeric)
sampled_time <- sample(crash_time_numeric,5000,replace=TRUE)

shapiro.test(sampled_time)
barplot(table(crashes_data$CRASH.TIME))

##NORMALITY TEST

#testing crash date
set.seed(1234)
dplyr::sample_n(crash_dates_no_year, 10)
library("ggpubr")
ggdensity(crash_dates_no_year, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")

library(ggpubr)
ggqqplot(crash_dates_no_year)


crash_dates_no_year_scaled <- scale(crash_dates_no_year);

summary(crash_dates_no_year_scaled);

hist(crash_dates_no_year);
hist(crash_dates_no_year_scaled);

hist(monthOfCrash);
hist(monthOfCrash_scaled);
