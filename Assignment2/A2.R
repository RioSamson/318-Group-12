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
library(forecast)

timeSeries <- ts(dataset2$, start=c(2007, 1), end = c(2007, 2))

# MA_m7 = forecast::ma(NDVI)

