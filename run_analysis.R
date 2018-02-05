
## The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
## The goal is to prepare tidy data that can be used for later analysis.
## One of the most exciting areas in all of data science right now is wearable computing - see for example this article. 
## Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
## The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.

## setwd("C:/Users/36762/Documents/Coursera2/Getting_and_cleaning_data/week4/Assignment")
## getwd()
## list.files()
## source = ("C:/Users/36762/Documents/Coursera2/Getting_and_cleaning_data/week4/Assignment/run_analysis.R")

library(dplyr)

## Download and get the data

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destZip <- "dataset.zip"
download.file(zipUrl, destZip, mode = "wb" )
dataUnzip <- "UCI HAR Dataset"
if(!file.exists(dataUnzip)) {
		unzip(destZip)
		}
		
## Read the data
# train data
trainSubjects <- read.table(file.path(dataUnzip, "train", "subject_train.txt"))
trainValues <- read.table(file.path(dataUnzip, "train", "X_train.txt"))
trainActivity <- read.table(file.path(dataUnzip, "train", "y_train.txt"))

# test data
testSubjects <- read.table(file.path(dataUnzip, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataUnzip, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataUnzip, "test", "y_test.txt"))

# features, don't convert text labels to factors
features <- read.table(file.path(dataUnzip, "features.txt"), as.is = TRUE)
 

# Activity labels
activities <- read.table(file.path(dataUnzip, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

## 1. Merges the training and the test sets to create one data set.

# Concatenate individual data tables to make single data table
humanActivity <- rbind(
		cbind(trainSubjects, trainValues, trainActivity), cbind(testSubjects, testValues, testActivity)
				)

# Assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")	
	
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# determine columns of data set to keep based on column name
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

# To keep data in these columns only
humanActivity <- humanActivity[, columnsToKeep]	
	
## 3. Uses descriptive activity names to name the activities in the data set

# Replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, levels = activities[, 1], labels = activities[, 2])	
	
## 4. Appropriately labels the data set with descriptive variable names.

# To get column names
humanActivityCols <- colnames(humanActivity)

# To remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# Expanding abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# To use new labels as column names
colnames(humanActivity) <- humanActivityCols
	
	
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Group by subject and activity and summarize using mean
humanActivityMeans <- humanActivity %>% 
						group_by(subject, activity) %>%
							summarise_each(funs(mean))

# Write the file  as "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)