Getting and Cleaning Data Course Project
==========


The run_analisys.R script contains a function called "run_analisys"" that processes the data available at: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip and transform the data to a new tidy data frame that is saved by default to run_analysys.txt.

The run_analysis() function does the following:

1. Download the data from the url if not already downloaded.

2. Extract data from all files from the archive as decribed in README.txt file from the achive from the provided url and merges the training and the test sets to create one data set and also appropriately labels the data set with descriptive variable names.

3. Extracts only the measurements on the mean and standard deviation for each measurement. 

4. Uses descriptive activity names to name the activities in the data set

5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

6. Save data from 5. to a destination file (by default it is run_analysis.txt)

Usage:

>run_analisys(zipFilePath="UCI_HAR_Dataset.zip", destFile="run_analysis.txt")

