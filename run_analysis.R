library(dplyr)
library(readr)
library(magrittr)
library(tidyr)

# Setting working directory and reading the various text files into R
setwd("C:/Users/brian/OneDrive/Brian/Data Science - Foundations of R Specialisation/Getting and Cleaning Data in R/week 4 assignment - getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")
activity_labels <- read.table("activity_labels.txt", F, sep = " ")
features <- read.table("features.txt", F , sep = "")
subject_test <- read.table("subject_test.txt", F, sep = "")
subject_train <- read.table("subject_train.txt", F , sep = "")
X_test <- read.table("X_test.txt", F, sep = "")
Y_test <- read.table("y_test.txt", F , sep = "")
X_train <- read.table("X_train.txt", F, sep = "")
Y_train <- read.table("y_train.txt", F, sep = "")

# Naming the labels as "activity"
colnames(Y_train) <- "activity"
colnames(Y_test) <- "activity"

# Naming the variable as "volunteernum", representing the subject who performed the activities 
colnames(subject_test) <- "volunteernum"
colnames(subject_train) <- "volunteernum"

# Naming the variables with the appropriate measurement names
colnames(X_train) <- features$V2
colnames(X_test) <- features$V2

# Combining labels and observations for the respective datasets into two separate datasets, traindata and testdata
traindata <- bind_cols(X_train, Y_train, subject_train)
testdata <- bind_cols(X_test, Y_test, subject_test)

# Merging the training and the test sets to create one data set
data <- bind_rows(traindata, testdata)

# Extracting only mean and std measurements
relevantdata <- select(data, 562:563, contains("mean"), contains("std"))

# Using descriptive activity names to name the activities in the data set
relevantdata$activity <- case_when(relevantdata$activity %% 6 == 0 ~ "LAYING", relevantdata$activity %% 5 == 0 ~ "STANDING", relevantdata$activity %% 4 == 0 ~ "SITTING", relevantdata$activity %% 3 == 0~ "WALKING_DOWNSTAIRS", relevantdata$activity %% 2 == 0~ "WALKING_UPSTAIRS", relevantdata$activity %% 1 == 0~"WALKING")

# Creating new independent tidy data set with the average of each variable for each activity and each subject.
relevantdata <- group_by(relevantdata, activity, volunteernum)
independent <- summarise_all(relevantdata, mean)

# Saving as txt file
write.table(independent, file = "independent_data.txt", row.names = FALSE)
