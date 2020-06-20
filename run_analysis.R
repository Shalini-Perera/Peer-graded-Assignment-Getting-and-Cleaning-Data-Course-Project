
#install libraries

install.packages("data.table")
install.packages("dplyr")

library(data.table)
library(dplyr)

featureNames <- read.table("D:/SLIIT/Year 3/Cources/Getting data/UCI HAR Dataset/features.txt")
activityLabels <- read.table("D:/SLIIT/Year 3/Cources/Getting data/UCI HAR Dataset/activity_labels.txt", header = FALSE)

#Read training data

subjectTrain <- read.table("D:/SLIIT/Year 3/Cources/Getting data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("D:SLIIT/Year 3/Cources/Getting data/UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("D:/SLIIT/Year 3/Cources/Getting data/UCI HAR Dataset/train/X_train.txt", header = FALSE)

#Read test data

subjectTest <- read.table("D:/SLIIT/Year 3/Cources/Getting data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("D:SLIIT/Year 3/Cources/Getting data/UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("D:SLIIT/Year 3/Cources/Getting data/UCI HAR Dataset/test/X_test.txt", header = FALSE)


#--------------------Part1-----------------------------
  
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

#Naming the columns

colnames(features) <- t(featureNames[2])

#Merge the data

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#---------------------Part2----------------------------
  
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

extractedData <- completeData[,requiredColumns]
dim(extractedData)

#---------------------Part3----------------------------
  
 extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)


#---------------------Part4----------------------------
  
  # survey the data
  names(extractedData) 


# expand abbreviations and clean up names

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

names(extractedData)

#---------------------Part5----------------------------
  
  extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)


#Making a second tidy data set

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

# output to file "Tidy.txt"

write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
write.table(tidyData,"D:/SLIIT/Year 3/Cources/Getting data/Tidy.txt",row.name=FALSE,sep=" ")
