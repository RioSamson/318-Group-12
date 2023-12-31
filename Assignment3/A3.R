library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

#this is just for my computer. different for yours! CHANGE, DELETE!
# setwd("C:/Users/User/ Downloads")

# upload the data set and then make the time into POSIX local time objects
dataset <- read.csv("Group_Assignment_1_Dataset.txt", header = TRUE) 
# %>%
#   na.omit()
dataset$Time <- as.POSIXlt(dataset$Time, format="%H:%M:%S")
dataset$Date <- as.POSIXlt(dataset$Date, format = "%d/%m/%Y")
dataset$Time <-format(dataset$Time, "%H:%M:%S")

#make the DATE+TIME values together, can take out if needed. 
Right_Date <- as.POSIXct(paste(dataset$Date, dataset$Time), format = "%Y-%m-%d %H:%M:%S")
dataset$DateTime <- Right_Date

#divide the data into weeks starting with 1=monday, add this marker to the dataframe
dataset<-dataset %>%
  mutate(weekday = wday(dataset$Date,week_start = 1))

#pick time window 180mins=>3h, Monday,1PM-3PM
time_window<-subset(dataset,dataset$Date$wday == 1)
time_window<-subset(time_window,time_window$Time>="13:00:00" & time_window$Time<"15:00:00")

#adding the info of which week number the data is from
time_window <- time_window %>% mutate(Week = week(Date))

#removing the 53rd week because its just one day and not a whole week- dec 31
#now there is only 52 weeks
time_window <- subset(time_window, time_window$Week != 53)

#For global_active_power:
time_window$Global_active_power<-scale(time_window$Global_active_power)
time_window <- select(time_window, -c(Global_reactive_power, Voltage,Global_intensity,Sub_metering_1,Sub_metering_2,Sub_metering_3, weekday))

library(depmixS4)

# ntimes:The vector of independent time series lengths.

#we are only going to use 37 weeks to train. This will be 70% for training + 30 for testing. - this is only for the term project

BIC_Values <- list()
Log_Likelihood_values <- list()

# BIC_Values[1] <- 0 
# BIC_Values[2] <- 0
# Log_Likelihood_values[1] <-0
# Log_Likelihood_values[2] <-0
for(i in 3:16){
  model <- depmix(response = Global_active_power ~1, data =time_window, 
                  nstates =i, ntimes = c(120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 
                                         120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 
                                         120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 
                                         120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 
                                         120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 
                                         120, 120))
  
  fm <- fit(model)
  BIC_Values[i] <- BIC(fm) 
  Log_Likelihood_values[i] <- logLik(fm)
}
Nstate_BIC <- unlist(BIC_Values)
Nstate_logLik <-unlist(Log_Likelihood_values)

min_value <- min(Nstate_BIC, Nstate_logLik)
max_value <- max(Nstate_BIC, Nstate_logLik)

plot( 3:16, Nstate_BIC, 
      xlab = "Number of States", ylab = "BIC/Log likelihood Values",
      main = "BIC/log likelihood Values vs. Number of States", ty="b", ylim = c(min_value, max_value)
      , col='blue')

lines(3:16, Nstate_logLik, ty="b", col='red')

legend(x="topright" , legend = c("BIC Values", "Log-likelihood Values"), cex = 1.0, fill = c('blue', 'red'))


