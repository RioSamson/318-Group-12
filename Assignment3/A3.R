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
