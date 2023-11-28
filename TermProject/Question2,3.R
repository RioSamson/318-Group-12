library(caret)
library(dplyr)
library(lubridate)
library(depmixS4)
library(dostats)

data <- read.csv("Datasets/TermProjectData.txt", header = TRUE) %>% na.omit()

data$Time <- as.POSIXlt(data$Time, format="%H:%M:%S")
data$Date <- as.POSIXlt(data$Date, format = "%d/%m/%Y")
data$Time <-format(data$Time, "%H:%M:%S")

###################### Question 2 - Training HMM ######################################################################
# Choosing a weekday of Monday and a time window of 17:30 - 20:30

# Add weekday column
data <- data %>% mutate(Weekday = wday(data$Date,week_start = 1))
# Filter out the data so that we only have data from Monday 17:30 - 20:30
data <- subset(data, data$Weekday == 1)
data <- subset(data, data$Time >= "17:30:00" & data$Time < "20:30:00")
# Split the data into train and test sets
trainingSet <- subset(data, data$Date <= as.POSIXlt("25/04/2009", format = "%d/%m/%Y"))
testingSet <- subset(data, data$Date > as.POSIXlt("25/04/2009", format = "%d/%m/%Y"))

# Getting the number of observations in each day, so that we can pass in this vector when we call depmix()
trainingSet$Date <- format(trainingSet$Date, format = "%d/%m/%Y")
numOfObservations <- table(trainingSet$Date)
numOfObservations <- as.vector(numOfObservations)

testingSet$Date <- format(testingSet$Date, format = "%d/%m/%Y")
numOfObservationsTest <- table(testingSet$Date)
numOfObservationsTest <- as.vector(numOfObservationsTest)

# Remove columns that are not Global_active_power and Global_intensity
trainingSetCompact <- trainingSet[, c("Global_active_power", "Global_intensity")]
testingSetCompact <- testingSet[, c("Global_active_power", "Global_intensity")]
# Scale the data
trainingSetScaled <- scale(trainingSetCompact)
trainingSetScaled <- as.data.frame(trainingSetScaled)
testingSetScaled <- scale(testingSetCompact)
testingSetScaled <- as.data.frame(testingSetScaled)

BIC_Values <- list()
Log_Likelihood_values <- list() 

# Training with n = 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24 states
# Assuming the data follows a Gaussian distribution
i <- 4
while (i <= 24) {
  model <- depmix(list(Global_active_power~1, Global_intensity~1), nstates = i,
                  data = trainingSetScaled, family = list(gaussian(), gaussian()),
                  ntimes = numOfObservations)
  i <- i + 2
  fm <- fit(model)
  BIC_Values[i] <- BIC(fm)
  Log_Likelihood_values[i] <- logLik(fm)
}

# Building HMM Models for the TEST data so we can compare the log-likelihood values
Test_Loglike_values <- list()
j <- 4
while (j <= 24) {
  testModel <- depmix(list(Global_active_power~1, Global_intensity~1), nstates = j,
                      data = testingSetScaled, family = list(gaussian(), gaussian()),
                      ntimes = numOfObservationsTest)
  j <- j + 2
  test_fm <- fit(testModel, data = trainingSetScaled)
  fb <- forwardbackward(test_fm, return.all = FALSE)
  Test_Loglike_values[j] <- fb$logLike
}

########## 11 states 
testModel2 <- depmix(list(Global_active_power~1, Global_intensity~1), nstates = 11,
                     data = testingSetScaled, family = list(gaussian(), gaussian()),
                     ntimes = numOfObservationsTest)
test_fm <- fit(testModel2, data = trainingSetScaled)
fb <- forwardbackward(test_fm, return.all = FALSE)
test_loglike_11 <- fb$logLike
####### 13 states 
testModel2 <- depmix(list(Global_active_power~1, Global_intensity~1), nstates = 13,
                     data = testingSetScaled, family = list(gaussian(), gaussian()),
                     ntimes = numOfObservationsTest)
fb <- forwardbackward(testModel2, return.all = FALSE)
test_loglike_13 <- fb$logLike

################### Plotting the BIC and Log-likelihood values

Nstate_BIC <- unlist(BIC_Values)
Nstate_logLik <-unlist(Log_Likelihood_values)
Test_loglike <- unlist(Test_Loglike_values)
norm_Nstate_logLik <- c()
norm_Test_loglike <- c()

for (i in 1:11) {
  norm_Nstate_logLik[i] <- Nstate_logLik[i] / 123
}
for (i in 1:11) {
  norm_Test_loglike[i] <- Test_loglike[i] / 32
}

min_value <- min(Nstate_BIC, Nstate_logLik)
max_value <- max(Nstate_BIC, Nstate_logLik)
norm_min <- min(norm_Nstate_logLik, norm_Test_loglike)
norm_max <- max(norm_Nstate_logLik, norm_Test_loglike)

x_axis <- c(4,6,8,10,12,14,16,18,20,22,24)

plot(x_axis, Nstate_BIC, 
     xlab = "Number of States", ylab = "BIC/Log likelihood Values",
     main = "BIC/log likelihood Values vs. Number of States", ty="b", ylim = c(min_value, max_value)
     , xlim = c(4, 24), col='blue')

lines(x_axis, Nstate_logLik, ty="b", col='red')
lines(x_axis, Test_loglike, ty="b", col='green')
abline(h = 0)

legend(x="topright" , legend = c("BIC Values", "Train Log-like", "Test Log-like"), cex = 1.0, fill = c('blue', 'red', 'green'))

plot(x_axis, norm_Nstate_logLik, 
     xlab = "Number of States", ylab = "Log likelihood Values",
     main = "Normalized Log-likelihood Values", ty="b", ylim = c(norm_min, norm_max)
     , xlim = c(4, 24), col='blue')

lines(x_axis, norm_Test_loglike, ty="b", col='red')

legend(x="topleft" , legend = c("Train Log-like", "Test Log-like"), cex = 1.0, fill = c('blue', 'red'))

############### Question 3 ###########################################################################################################


a1_dataset <- read.csv("Datasets/Anomalous Datasets/DataWithAnomalies1.txt") %>% na.omit()
a2_dataset <- read.csv("Datasets/Anomalous Datasets/DataWithAnomalies2.txt") %>% na.omit()
a3_dataset <- read.csv("Datasets/Anomalous Datasets/DataWithAnomalies3.txt") %>% na.omit()

# Add weekday column
a1_dataset <- a1_dataset %>% mutate(Weekday = wday(a1_dataset$Date,week_start = 1))
a2_dataset <- a2_dataset %>% mutate(Weekday = wday(a2_dataset$Date,week_start = 1))
a3_dataset <- a3_dataset %>% mutate(Weekday = wday(a3_dataset$Date,week_start = 1))
# Filter out the data so that we only have data from Monday 17:30 - 20:30
a1_dataset <- subset(a1_dataset, a1_dataset$Weekday == 1)
a2_dataset <- subset(a2_dataset, a2_dataset$Weekday == 1)
a3_dataset <- subset(a3_dataset, a3_dataset$Weekday == 1)

a1_dataset <- subset(a1_dataset, a1_dataset$Time >= "17:30:00" & a1_dataset$Time < "20:30:00")
a2_dataset <- subset(a2_dataset, a2_dataset$Time >= "17:30:00" & a2_dataset$Time < "20:30:00")
a3_dataset <- subset(a3_dataset, a3_dataset$Time >= "17:30:00" & a3_dataset$Time < "20:30:00")

# Getting the number of observations in each day, so that we can pass in this vector when we call depmix()
a1_dataset$Date <- format(a1_dataset$Date, format = "%d/%m/%Y")
a2_dataset$Date <- format(a2_dataset$Date, format = "%d/%m/%Y")
a3_dataset$Date <- format(a3_dataset$Date, format = "%d/%m/%Y")
numOfObservations1 <- table(a1_dataset$Date)
numOfObservations2 <- table(a2_dataset$Date)
numOfObservations3 <- table(a3_dataset$Date)
numOfObservations1 <- as.vector(numOfObservations1)
numOfObservations2 <- as.vector(numOfObservations2)
numOfObservations3 <- as.vector(numOfObservations3)

# Remove columns that are not Global_active_power and Global_intensity
a1_compact <- a1_dataset[, c("Global_active_power", "Global_intensity")]
a2_compact <- a2_dataset[, c("Global_active_power", "Global_intensity")]
a3_compact <- a3_dataset[, c("Global_active_power", "Global_intensity")]
# Scale the data
a1_scaled <- scale(a1_compact)
a2_scaled <- scale(a2_compact)
a3_scaled <- scale(a3_compact)
a1_scaled <- as.data.frame(a1_scaled)
a2_scaled <- as.data.frame(a2_scaled)
a3_scaled <- as.data.frame(a3_scaled)

#### Anomalous dataset 1 ####
model <- depmix(list(Global_active_power~1, Global_intensity~1), nstates = 12,
                data = a1_scaled, family = list(gaussian(), gaussian()),
                ntimes = numOfObservations1)
fm <- fit(model)
fb <- forwardbackward(fm, return.all = FALSE)
a1_loglike <- fb$logLike

#### Anomalous dataset 2 ####
model <- depmix(list(Global_active_power~1, Global_intensity~1), nstates = 12,
                data = a2_scaled, family = list(gaussian(), gaussian()),
                ntimes = numOfObservations2)
fm <- fit(model)
fb <- forwardbackward(fm, return.all = FALSE)
a2_loglike <- fb$logLike

#### Anomalous dataset 3 ####
model <- depmix(list(Global_active_power~1, Global_intensity~1), nstates = 12,
                data = a3_scaled, family = list(gaussian(), gaussian()),
                ntimes = numOfObservations3)
fm <- fit(model)
fb <- forwardbackward(fm, return.all = FALSE)
a3_loglike <- fb$logLike

################################################################################################