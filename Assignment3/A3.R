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
