library(reshape2)
## Prepare tidy data that can be used for later analysis from existing
## Human Activity Recognition Using Smartphones Dataset 
## available at "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
run_analysis <- function(zipFilePath="UCI_HAR_Dataset.zip", destFile="run_analysis.txt"){
    ## ====== 1. Download the data from url if not already downloaded.
    
    ##check if the data file exists
    if(!file.exists(zipFilePath)){
        ## if not exist then we ill download 
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", method="curl",destfile = zipFilePath)
    }
    
    ## ====== 2. Merges the training and the test sets to create one data set and 
    ##           and appropriately labels the data set with descriptive variable names. 
    
    ## merge - total in standard gravity units 'g'. Every row shows a 128 element vector 
    totalAccX <- rbind(getTestTotalAccData("x",zipFilePath=zipFilePath),
                      getTrainTotalAccData("x",zipFilePath=zipFilePath))
    ## rename all variables
    names(totalAccX) <- paste("totalX",1:dim(totalAccX)[2], sep="")
    
    totalAccY <- rbind(getTestTotalAccData("y",zipFilePath=zipFilePath),
                      getTrainTotalAccData("y",zipFilePath=zipFilePath))
    names(totalAccY) <- paste("totalY",1:dim(totalAccY)[2], sep="")
  
    totalAccZ <- rbind(getTestTotalAccData("z",zipFilePath=zipFilePath),
                      getTrainTotalAccData("z",zipFilePath=zipFilePath))
    names(totalAccZ) <- paste("totalZ",1:dim(totalAccZ)[2], sep="")
    
    ## merge - body accelerations
    bodyAccX <- rbind(getTestBodyAccData("x",zipFilePath=zipFilePath),
                      getTrainBodyAccData("x",zipFilePath=zipFilePath))
    ## rename variables
    names(bodyAccX) <- paste("accX",1:dim(bodyAccX)[2], sep="")
    
    bodyAccY <- rbind(getTestBodyAccData("y",zipFilePath=zipFilePath),
                      getTrainBodyAccData("y",zipFilePath=zipFilePath))
    ## rename variables
    names(bodyAccY) <- paste("accY",1:dim(bodyAccY)[2], sep="")
    
    bodyAccZ <- rbind(getTestBodyAccData("z",zipFilePath=zipFilePath),
                      getTrainBodyAccData("z",zipFilePath=zipFilePath))
    ## rename variables
    names(bodyAccZ) <- paste("accZ",1:dim(bodyAccZ)[2], sep="")
    
    ## merge - body angular velocity
    bodyGyroX <- rbind(getTestBodyGyroData("x",zipFilePath=zipFilePath),
                   getTrainBodyGyroData("x", zipFilePath=zipFilePath))
    ## rename variables
    names(bodyGyroX) <- paste("gyroX",1:dim(bodyGyroX)[2], sep="")
    
    bodyGyroY <- rbind(getTestBodyGyroData("y",zipFilePath=zipFilePath),
                   getTrainBodyGyroData("y", zipFilePath=zipFilePath))
    ## rename variables
    names(bodyGyroY) <- paste("gyroY",1:dim(bodyGyroY)[2], sep="")
    
    bodyGyroZ <- rbind(getTestBodyGyroData("z",zipFilePath=zipFilePath),
                   getTrainBodyGyroData("z", zipFilePath=zipFilePath))
    ## rename variables
    names(bodyGyroZ) <- paste("gyroZ",1:dim(bodyGyroZ)[2], sep="")
    
    subject <- rbind(getTestSubjectData(zipFilePath=zipFilePath),
                     getTrainSubjectData(zipFilePath = zipFilePath))
    names(subject) <- "subject"
    
    ## merge - test sets
    X <- rbind(getTestXData(zipFilePath=zipFilePath),
               getTrainXData(zipFilePath=zipFilePath))
    ## extract names from features and rename the variables according with
    names(X) <- getFeaturesData()$V2
    
    ## merge - activities
    y <- rbind(getTestYData(zipFilePath=zipFilePath),
               getTrainYData(zipFilePath=zipFilePath))
    # rename variable
    names(y) <- "activity"
    
    ## merge all together
    data <- cbind(subject,y,X,totalAccX,totalAccY,totalAccZ,
                  bodyAccX,bodyAccY,bodyAccZ,
                  bodyGyroX,bodyGyroY,bodyGyroZ)
    
    ## ====== 3. Extracts only the measurements on the mean and standard 
    ##           deviation for each measurement. 
  
    ## keep only subject (1), activity (2) and variables that have std() and mean() in name
    dataNames <- names(data)
    data <- data[,c(1,2,grep("mean()",dataNames,fixed = TRUE),
                    grep("std()",dataNames,fixed = TRUE))]
    
    ## ====== 4. Uses descriptive activity names to name the activities in the data set
    ## factorize activities
    activ <- getActivityLabelsData()
    data$activity <- factor(data$activity, levels=activ$V1, labels=activ$V2)
    ## factorize subjects
    data$subject <- factor(data$subject)
    
    ## ====== 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
    ## for each activity and each subject.
    ## convert data to a molten data frame by subject and activity variable
    meltData <- melt(data, id.vars = c("subject", "activity"))
    ## cast the molten data frame variables by each subject and activity  
    dataMean <- dcast( meltData, subject + activity ~ variable , mean)
    
    ## ====== 6. Save data from 5. to a destination file (by default it is run_analysis.txt)
    write.table(dataMean,file=destFile,row.names=FALSE) 
    ## display info about saved object
    print(paste("Saved ",ceiling(object.size(data)/1024),"KB object to: ",destFile, sep = ""))
    #dataMean
}

## NAMES 
## ==================================================
## Helper methods to get common variables names and/or labels
getFeaturesData <- function(zipFilePath="UCI_HAR_Dataset.zip"){
    ## List of all features.
    # read 561 variable names
    readDataFromZipPathFile(zipFilePath, "UCI HAR Dataset/features.txt")
}
getActivityLabelsData <- function(zipFilePath="UCI_HAR_Dataset.zip"){
    ## Links the class labels with their activity name.
    # read 6 levels
    readDataFromZipPathFile(zipFilePath, "UCI HAR Dataset/activity_labels.txt")
}

## DATA 

## ===================================================
## Helper funcions for getting test data i.e. 30% of the volunteers
getTestXData <- function(zipFilePath="UCI_HAR_Dataset.zip"){
    ## Test set. 561 variables with time and frequency domain variables used as 
    ## defined in features.txt
    # 2947 obs of 561 variables = 1653267 items
    readDataFromZipPathFile(zipFilePath, "UCI HAR Dataset/test/X_test.txt")
}

getTestSubjectData <- function(zipFilePath="UCI_HAR_Dataset.zip"){
    ## Each row identifies the subject who performed the activity for each window sample.
    ## Its range is from 1 to 30.
    # 2947 items
    readDataFromZipPathFile(zipFilePath, "UCI HAR Dataset/test/subject_test.txt")
}
getTestYData <- function(zipFilePath="UCI_HAR_Dataset.zip"){
    ## Test labels. Posbile values are defined in activity_labels.txt
    # 2947 items
    readDataFromZipPathFile(zipFilePath, "UCI HAR Dataset/test/y_test.txt")
}

getTestTotalAccData <- function(coord,zipFilePath="UCI_HAR_Dataset.zip"){
    ## The acceleration signal from the smartphone accelerometer <coord> axis
    ## in standard gravity units 'g'. Every row shows a 128 element vector i.e.
    ## 128 readings/window
    ## 2947 items
    readDataFromZipPathFile(zipFilePath,
                            paste( "UCI HAR Dataset/test/Inertial Signals/total_acc_",
                                  coord,"_test.txt",sep=""))
}
getTestBodyAccData <- function(coord,zipFilePath="UCI_HAR_Dataset.zip"){
    ## The body acceleration signal obtained by subtracting 
    ## the gravity from the total acceleration for <coord> axis.
    readDataFromZipPathFile(zipFilePath,
                            paste("UCI HAR Dataset/test/Inertial Signals/body_acc_",
                                  coord,"_test.txt",sep=""))
}
getTestBodyGyroData <- function(coord,zipFilePath="UCI_HAR_Dataset.zip"){
    ## The angular velocity vector measured by the gyroscope for each window 
    ## sample for <coord> axis. The units are radians/second. 
    readDataFromZipPathFile(zipFilePath,
                            paste("UCI HAR Dataset/test/Inertial Signals/body_gyro_",
                                  coord,"_test.txt",sep=""))
    
}
## ===========================================
## Helper funcions for getting training data  i.e. 70% of volunteers
getTrainXData <- function(zipFilePath="UCI_HAR_Dataset.zip"){
    ## Train set. 561 variables with time and frequency domain variables used as 
    ## defined in features.txt
    # 2947 obs of 561 variables = 1653267 items
    readDataFromZipPathFile(zipFilePath, "UCI HAR Dataset/train/X_train.txt")
}
getTrainSubjectData <- function(zipFilePath="UCI_HAR_Dataset.zip"){
    ## Each row identifies the subject who performed the activity for each window sample.
    ## Its range is from 1 to 30.
    ## 
    # 7352 items
    readDataFromZipPathFile(zipFilePath, "UCI HAR Dataset/train/subject_train.txt")
}
getTrainYData <- function(zipFilePath="UCI_HAR_Dataset.zip"){
    ## Training labels. Posbile values are defined in activity_labels.txt
    # 2947 items
    readDataFromZipPathFile(zipFilePath, "UCI HAR Dataset/train/y_train.txt")
}
# 7352 obs of 128 variables = 941056 items
getTrainTotalAccData <- function(coord,zipFilePath="UCI_HAR_Dataset.zip"){
    ## The acceleration signal from the smartphone accelerometer <coord> axis in 
    ## standard gravity units 'g'. Every row shows a 128 element vector
    ## i.e. 128 readings/window
    readDataFromZipPathFile(zipFilePath,
                            paste("UCI HAR Dataset/train/Inertial Signals/total_acc_",
                                  coord,"_train.txt",sep=""))
}

getTrainBodyAccData <- function(coord,zipFilePath="UCI_HAR_Dataset.zip"){
    ##The body acceleration signal obtained by subtracting the gravity
    ## from the total acceleration for <coord> axis. 
    readDataFromZipPathFile(zipFilePath,
                            paste("UCI HAR Dataset/train/Inertial Signals/body_acc_",
                                  coord,"_train.txt",sep=""))
}

getTrainBodyGyroData <- function(coord,zipFilePath="UCI_HAR_Dataset.zip"){
    ## The angular velocity vector measured by the gyroscope for each window sample 
    ## for <coord> axis. The units are radians/second. 
    readDataFromZipPathFile(zipFilePath,
                            paste("UCI HAR Dataset/train/Inertial Signals/body_gyro_",
                                  coord,"_train.txt",sep=""))
}

readDataFromZipPathFile <- function(zipPath = "UCI_HAR_Dataset.zip", file){
    con <- unz(zipPath,file)
    open(con)
    data <- read.table(con)
    close(con)
    
    data
}
