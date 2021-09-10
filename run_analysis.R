##################################################################################################

library(dplyr)

# 0. Download and read data
## Downloading and unzipping file
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
        download.file(zipUrl, zipFile, mode = "wb")
}

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
        unzip(zipFile)
}


## Read data
trainSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

## Read features
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)


## Read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")



# 1. Merge the training and the test sets to create one data set.
## Concatenate individual data files
activityData <- rbind(
        cbind(trainSubjects, trainValues, trainActivity),
        cbind(testSubjects, testValues, testActivity)
)

## Assign column names
colnames(activityData) <- c("subject", features[, 2], "activity")



# 2.Extract mean and standard deviation for each measurement. 
## Filter column names
colKeep <- grepl("subject|activity|mean|std", colnames(activityData))

# Keep only the columns containing "subject", "activity", "mean" or "std in the data set
activityData <- activityData[, colKeep]

# Remove individual files from global environment
rm(trainSubjects, trainValues, trainActivity, 
   testSubjects, testValues, testActivity)



## 3. Use descriptive activity names to name the activities 
##    in the data set.
# Replace activity values with named factor levels
activityData$activity <- factor(activityData$activity, 
                                 levels = activities[, 1], labels = activities[, 2])



## 4. Label data set with descriptive variable names.
# Get column names
activityDataCols <- colnames(activityData)

# Remove special characters
activityDataCols <- gsub("[\\(\\)-]", "", activityDataCols)

# Clean up names
activityDataCols <- gsub("^f", "frequencyDomain", activityDataCols)
activityDataCols <- gsub("^t", "timeDomain", activityDataCols)
activityDataCols <- gsub("Acc", "Accelerometer", activityDataCols)
activityDataCols <- gsub("Gyro", "Gyroscope", activityDataCols)
activityDataCols <- gsub("Mag", "Magnitude", activityDataCols)
activityDataCols <- gsub("Freq", "Frequency", activityDataCols)
activityDataCols <- gsub("mean", "Mean", activityDataCols)
activityDataCols <- gsub("std", "StandardDeviation", activityDataCols)

# Correct spelling error
activityDataCols <- gsub("BodyBody", "Body", activityDataCols)

# Assign new labels as column names
colnames(activityData) <- activityDataCols


## 5. Create independent tidy data set with average of each variable 
##    for each activity and each subject.
# Group activityData by subject and activity and summarize by mean
activityDataMeans <- activityData %>% 
        group_by(subject, activity) %>%
        summarise_each(funs(mean))

# Write output to the table "tidy_data.txt"
write.table(activityDataMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

####################################################################################################
