#imports
library(dplyr)
library(depmixS4)

#this is just for my computer. different for yours! CHANGE, DELETE!
setwd("C:/Users/User/Downloads/Datasets")

# copying dataset again for question 2  ***na is not omitted
# initial_data <- read.csv("TermProjectData.txt", header = TRUE)  
# initial_data$id <- 1:nrow(initial_data)

#-----------------------QUESTION 1--------------------------------

# upload the data set and then make the time into POSIX local time objects
dataset <- read.csv("TermProjectData.txt", header = TRUE) %>% na.omit()

#setting the time format
dataset$Time <- as.POSIXlt(dataset$Time, format="%H:%M:%S")
dataset$Date <- as.POSIXlt(dataset$Date, format = "%d/%m/%Y")
dataset$Time <-format(dataset$Time, "%H:%M:%S")

#create ID column
dataset$id <- 1:nrow(dataset)

#set seed so that the data is reproducible
set.seed(1)

#patitioning the data into training and test data
split_index <- round(0.8 * nrow(dataset)) # Calculate the index for the split
train_data <- dataset[1:split_index, ]
test_data <- dataset[(split_index + 1):nrow(dataset), ]

# train_data <- train_data %>% select(-c(Date, Time, id))
# train_data <- scale(train_data)

# Feature selection and exclusion
train_data <- train_data %>% select(-c(Date, Time, id))

# Scale the numeric training data using standardization
scaled_train_data <- scale(train_data)

# Apply PCA to the scaled training data
pca <- prcomp(scaled_train_data, center = TRUE, scale. = TRUE)

#making the biplot
library(devtools)
install_github("vqv/ggbiplot", force = TRUE)
require(ggbiplot)
# ggbiplot(pca);

#ggplot of a subset of the scaled data
subset_size <- 5000
subset_scaled_train_data <- scaled_train_data[1:subset_size, ]
pca2 <- prcomp(subset_scaled_train_data, center = TRUE)
ggbiplot(pca2, alpha = 0) + theme(legend.direction = 'horizontal', legend.position = 'top')


#making a bar graph and line graph for PC as shown in the video
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot of PC 1-7", xlab="Principle Component", ylab="Percent Variation")
screeplot(pca, type = "line", main = "Scree plot of PC 1-7" )

# Identify features with the highest absolute loading values for each PC
pca <- prcomp(scaled_train_data, center = TRUE, scale. = TRUE)
rotation_matrix <- pca$rotation
summary(pca)

print(rotation_matrix)