# Getting and Cleaning Data peer-review assignment
This repository contains the following files:

README.md, this file, which provides an overview of the data set and how it was created. tidy_data.txt, which contains the data set. CodeBook.md, the code book, which describes the contents of the data set (data, variables and transformations used to generate the data). run_analysis.R, the R script that was used to create the data set.

1. Study design

This project is based on the Human Activity Recognition Using Smartphones Dataset, which describes how the data was initially collected as follows:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 


2. Create the data set

The R script run_analysis.R can be used to create the data set. It retrieves the source data set and transforms it to produce the final data set by implementing the following steps:

Download and unzip source data if it doesn't exist. Read data. Merge the training and the test sets to create one data set. Extract the measurements on the mean and standard deviation for each measurement. Use descriptive activity names to name the activities. Label the data set with descriptive variable names. Create a second, independent tidy set with the average of each variable for each activity and each subject. Write the data set to the tidy_data.txt file. The tidy_data.txt in this repository was created by running the run_analysis.R script using R version 4.1.0 (2021-05-18) on Windows 7 64-bit edition.

This script requires the dplyr package (version 1.0.7 was used).
