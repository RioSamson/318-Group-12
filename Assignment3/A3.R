<<<<<<< HEAD
# library(lubridate)
# library(dplyr)
# library(lubridate)
# library(tidyr)
# library(ggplot2)


setwd("C:/Users/User/Downloads")
dataset <- read.table("Group_Assignment_1_Dataset.txt", header=TRUE, sep=",")

#converting time and date from a string to Posixct objects for calculations
dataset$Date <- as.POSIXlt(dataset$Date, format = "%d/%m/%Y")
dataset$Time <- as.POSIXlt(dataset$Time, format="%H:%M:%S")

#make the date column have the right DATE+TIME values together
Right_Date <- as.POSIXct(paste(dataset$Date, format(dataset$Time, "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S")
dataset$Time <- Right_Date
=======
library(lubridate)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

dataset <- read.csv("Group_Assignment_3_Dataset.txt", header = TRUE) %>%
  na.omit()
dataset$Time <- as.POSIXlt(dataset$Time, format="%H:%M:%S")
dataset$Date <- as.POSIXlt(dataset$Date, format = "%d/%m/%Y")
dataset$Time <-format(dataset$Time, "%H:%M:%S")
dataset<-dataset %>%
  mutate(weekday = wday(dataset$Date,week_start = 1))
#pick time window 180mins=>3h, Monday,1PM-3PM
time_window<-subset(dataset,dataset$Date$wday == 1)
time_window<-subset(time_window,time_window$Time>="13:00:00" & time_window$Time<"15:00:00")

#For global_active_power:
time_window$Global_active_power<-scale(time_window$Global_active_power)
time_window <- select(time_window, -c(Global_reactive_power, Voltage,Global_intensity,Sub_metering_1,Sub_metering_2,Sub_metering_3))
>>>>>>> ec6c60d5a17a71400945b3aba010206a86faa44e
