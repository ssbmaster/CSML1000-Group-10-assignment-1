raw_crashesData <- read.csv('./data/Motor_Vehicle_Collisions_-_Crashes.csv')
#clean_crashesData <- read.csv('./data/motor_vehicle_collisions_crashes_cleaned.csv')
library(ggplot2) #for plotting graphs
library(RColorBrewer) #for organizing colour use with palettes
outlineColour = brewer.pal(9, "Set1")
fillColour = brewer.pal(9, "Pastel1")

raw_crash_dates <- as.Date(raw_crashesData$CRASH.DATE, "%m/%d/%Y")
raw_crash_dates_df <- as.data.frame(raw_crash_dates)

clean_crash_dates <- raw_crash_dates
clean_crash_dates <- format(clean_crash_dates, format="%m/%d")
clean_crash_dates <- as.Date(clean_crash_dates, "%m/%d")
clean_crash_dates_df <- as.data.frame(clean_crash_dates)

#density plot of dates with all years
plot_rawCrashDates <- ggplot(raw_crash_dates_df, aes(x=raw_crash_dates_df$raw_crash_dates)) + geom_density(fill=fillColour[1], colour=outlineColour[1], alpha = 0.4) + 
  geom_vline(aes(xintercept=as.numeric(as.Date("2013-01-01"))),color="black", linetype="solid", size=1) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2014-01-01"))),color="black", linetype="solid", size=1) + 
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-01-01"))),color="black", linetype="solid", size=1) + 
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-01-01"))),color="black", linetype="solid", size=1) + 
  geom_vline(aes(xintercept=as.numeric(as.Date("2017-01-01"))),color="black", linetype="solid", size=1) + 
  geom_vline(aes(xintercept=as.numeric(as.Date("2018-01-01"))),color="black", linetype="solid", size=1) + 
  geom_vline(aes(xintercept=as.numeric(as.Date("2019-01-01"))),color="black", linetype="solid", size=1) + 
  geom_vline(aes(xintercept=as.numeric(as.Date("2020-01-01"))),color="black", linetype="solid", size=1)

#density plot of all dates with combined year
plot_cleanCrashDates <- ggplot(clean_crash_dates_df, aes(x=clean_crash_dates_df$clean_crash_dates)) + geom_density(fill=fillColour[1], colour=outlineColour[1], alpha = 0.4)

plot_rawCrashDates
plot_cleanCrashDates