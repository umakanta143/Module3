setwd('C:/Users/usahoo/Documents/coursera/UCI HAR Dataset')
# Initialize library used
library(data.table)
library(dplyr)

# Read Supporting Metadata
featureNames <- read.table("features.txt")
activityLabels <- read.table("activity_labels.txt", header = FALSE)

#Read the training data
subjectTrain <- read.table("train/subject_train.txt", header = FALSE)
activityTrain <- read.table("train/y_train.txt", header = FALSE)
featuresTrain <- read.table("train/X_train.txt", header = FALSE)


# Read the test data
subjectTest <- read.table("test/subject_test.txt", header = FALSE)
activityTest <- read.table("test/y_test.txt", header = FALSE)
featuresTest <- read.table("test/X_test.txt", header = FALSE)

# Merge the training and the test sets to create one data set

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# Naming the columns
colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

## 1. Merges the training and the test sets to create one data set.
MergedData <- cbind(features,activity,subject)

##2.Extracts only the measurements on the mean and standard deviation for each measurement.

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(MergedData), ignore.case=TRUE)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
#dim(MergedData)
extractedData<-MergedData[,requiredColumns]
#dim(extractedData)

##3.Uses descriptive activity names to name the activities in the data set
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)


##4.Appropriately labels the data set with descriptive variable names.
colnames(extractedData)<-gsub("Acc","Accelerometer",colnames(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)

##5.From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)
tidydata<-aggregate(.~Subject+Activity,extractedData,mean)
write.table(tidydata, file = "tidy_data.txt", row.names = FALSE)
