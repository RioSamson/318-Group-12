library(caret)
library(dplyr)
library(lubridate)
library(depmixS4)

#Take care of N/A values (elimination or interpolation). 
dataset <- read.csv("Datasets/TermProjectData.txt", header = TRUE) %>%
  na.omit()

# copying the initial dataset into another variable
initial_data <- data.frame(dataset)

dataset$Time <- as.POSIXlt(dataset$Time, format="%H:%M:%S")
dataset$Date <- as.POSIXlt(dataset$Date, format = "%d/%m/%Y")
dataset$Time <-format(dataset$Time, "%H:%M:%S")

#create ID column
dataset$id <- 1:nrow(dataset)
# Create the partition
splitIndex <- createDataPartition(dataset$id, p = 0.8, list = FALSE, times = 1)

# Split the data into training and test sets
trainingSet <- dataset[splitIndex, ]
testSet <- dataset[-splitIndex, ]

#delete not numerical variables
trainingSet_numer<- trainingSet %>%dplyr::select(-c(Date, Time, id))

#scale training set
train_scaled<-scale(trainingSet_numer)
# # Tried keeping Date column so that I could add a weekday column, but Date is not numeric, causes problems
# If I add the weekday column too early like on line 15, then the scaling messes up those numbers by the time 
# I need to use them (i get values like 1.0032, 0.0992, -0.879, etc)
# How to label a row as a specific day of the week?
# Are we supposed to be partitioning the scaled data again? Cuz we partitioned into train and test THEN scaled 
# Do we just use the already scaled data and somehow capture a time window? (ex. Monday, 5:30pm - 8:30pm)

# Performing PCA
pca_result <- prcomp(train_scaled, center = TRUE, scale. = TRUE)
# Summarizing PCA results
summary(pca_result)
# Plotting Scree plot
plot(pca_result, type = "lines")

loadings <- pca_result$rotation
# Displaying loading scores of the first three principal components
variable_matrix<-loadings[, 1:3]
abs_matrix <- abs(variable_matrix)
# Find the maximum value in first column(PCA1)
######
#based on the loading matrix(choose PC1->40.85%), find out which variables have large percentage：
#Global_active_power,Global_intensity
######