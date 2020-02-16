#Read the data in
crashes_data_read <- read.csv("Motor_Vehicle_Collisions_-_Crashes.csv")

#Invoke library dplyr
library(dplyr)       

#Feature engineering
#crashes_data_good <- data.frame(crashes_data_read[!is.na(crashes_data_read$LATITUDE) & !is.na(crashes_data_read$LONGITUDE),]) #filter out all zero Latitude & Longitude rows
crashes_data_columns <- crashes_data_read %>% select(CRASH.DATE, CRASH.TIME, LONGITUDE, LATITUDE, COLLISION_ID)
crashes_data_columns$CRASH.DATE <- as.Date(crashes_data_columns$CRASH.DATE, format="%m/%d/%Y")
#min(crashes_data_good1$LATITUDE)

crashes_data_bound <- subset.data.frame(crashes_data_columns, crashes_data_columns$LATITUDE != "0" & crashes_data_columns$LONGITUDE != "0" & crashes_data_columns$LATITUDE >= "40.49" & crashes_data_columns$LATITUDE <= "40.92" & crashes_data_columns$LONGITUDE <= "-74.26" & crashes_data_columns$LONGITUDE >= "-73.67") #This is a serious bug in R

library(rgeos)
library(sp)
library(sf)
library(rgdal)


precinct.map <- readOGR(".", layer = "geo_export_32d06294-3e95-408c-86e3-7a17a84f9c0e")

#xy <- data.frame(Longitude = crashes_data_bound$LONGITUDE, Latitude = crashes_data_bound$LATITUDE) 
spdf <- SpatialPointsDataFrame(coords = crashes_data_bound[,c(3,4)], data = crashes_data_bound[,c(3,4)], proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

proj4string(spdf) <- proj4string(precinct.map)
crashes_data_bound$precinct <- (over(spdf, precinct.map))

library(lubridate)
x <- as.POSIXct(crashes_data_bound$CRASH.TIME, format = "%H:%M")
round_date(x, "30 minutes")
crashes_data_bound$TIME <- strftime(round_date(x, "30 minutes"), format = "%H:%M")

#to separate data frame from within another data frame
crashes_data_filter <- data.frame(CRASH.DATE = crashes_data_bound$CRASH.DATE, CRASH.TIME = crashes_data_bound$TIME, COLLISION.ID = crashes_data_bound$COLLISION_ID, PRECINCT = crashes_data_bound$precinct$precinct)

crashes_data_pivot <- crashes_data_filter %>% select(CRASH.DATE, CRASH.TIME, PRECINCT) %>% group_by(CRASH.DATE, CRASH.TIME) %>% count(PRECINCT)
names(crashes_data_pivot)[names(crashes_data_pivot) == "n"] <- "COLLISION.COUNT"

library(tidyr)
crashes_data_clean <- as.data.frame(crashes_data_pivot %>% separate(CRASH.DATE, c("Year", "Month", "Day"), sep = "-"))
#crashes_data_clean[crashes_data_clean$COLLISION.COUNT == 9,5]=19 (Done in the console, as it is a one time fix)

crashes_data_tidy <- crashes_data_clean %>% select(Month, Day, CRASH.TIME, PRECINCT, COLLISION.COUNT)

crashes_data_tidy1 <- as.data.frame(crashes_data_tidy %>% select(Month, Day, CRASH.TIME, PRECINCT, COLLISION.COUNT) %>% group_by(Month, Day, CRASH.TIME, PRECINCT) %>% summarise(TOTAL.COLLISION = sum(COLLISION.COUNT)))
crashes_data <- data.frame(Month = as.integer(crashes_data_tidy1$Month), Day = as.integer(crashes_data_tidy1$Day), CRASH.TIME = crashes_data_tidy1$CRASH.TIME, PRECINCT = as.character(crashes_data_tidy1$PRECINCT), TOTAL.COLLISION = crashes_data_tidy1$TOTAL.COLLISION)

library(data.table)
library(mltools)


#train, test split
library(caTools)
set.seed(123)

crashes_data_sample <- sample.split(crashes_data$TOTAL.COLLISION, SplitRatio = 0.8)
crashes_data_train <- subset(crashes_data, crashes_data_sample==TRUE)
crashes_data_test <- subset(crashes_data, crashes_data_sample==FALSE)

#Fitting the multiple regression model
library(rpart)
library(rpart.plot)

model <- rpart(TOTAL.COLLISION ~ PRECINCT + CRASH.TIME, data = crashes_data_train)

summary(model)
rpart.plot(model)


library(pROC)
regtree.predict <- predict(model, crashes_data_test)

auc.regtree = roc(crashes_data_test$TOTAL.COLLISION, regtree.predict, plot = TRUE, col = "blue")
print(auc.regtree)


