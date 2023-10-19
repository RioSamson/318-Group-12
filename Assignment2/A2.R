setwd("C:/Users/User/Downloads")
dataset <- read.table("Group_Assignment_1_Dataset.txt", header=TRUE, sep=",")

#converting time and date from a string to Posixct objects for calculations
dataset$Time <- as.POSIXlt(dataset$Time, format="%H:%M:%S")
dataset$Date <- as.POSIXlt(dataset$Date, format = "%d/%m/%Y")
dataset_week12 <- subset(dataset, dataset$Date >=  as.POSIXlt("19/3/2007", format="%d/%m/%Y") & dataset$Date <= as.POSIXlt("25/3/2007", format="%d/%m/%Y"))

#making duplicate dataset for calculations
dataset2 <- dataset
dataset2$Date <- as.Date(dataset$Date, format = "%Y-%m-%d")

library(lubridate)
library(dplyr)
library(magrittr)
library(ISOweek)

#spliting the the dataset into different weeks
#January 1, 2007 is a monday
#make dataset2 have an extra filed that has the week number
#2007 starts with a monday, has 52 full weeks, plus december 31 (week 53)
dataset2 <- dataset2 %>% mutate(Week = week(Date))
grouped_data <- dataset2 %>% group_by(Week)

#calculating the moving average
# library(forecast)
library(zoo)
dataset2 <- subset(dataset2, dataset2$Week != 53, select = c("Date", "Time" , "Global_intensity", "Week"))
movingAveList <- list()

for(i in 1:52) {
  week_subset <- subset(dataset2, dataset2$Week == i)
  timeSeries <- ts(week_subset)
  
  week_movingAve <- rollmean(timeSeries, k=10, align='right')
  
  movingAveList[[i]] <- week_movingAve
}

# Create a new data frame to store only the Global_intensity columns from movingAveList
global_intensity_data <- data.frame(matrix(nrow = 10071, ncol = 52))  # Initialize an empty data frame

# Populate the data frame with Global_intensity values
for (j in 1:52) {
  global_intensity_data[, j] <- movingAveList[[j]][, 3]
}

global_intensity_data$Row_Averages <- rowMeans(global_intensity_data, na.rm = TRUE)







