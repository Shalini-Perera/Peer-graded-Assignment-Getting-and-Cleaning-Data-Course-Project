Getting and Cleaning Data - peer assessment project

The original data was transformed by

Merging the training and the test sets to create one data set.
Extracting only the measurements on the mean and standard deviation for each measurement.
Using descriptive activity names to name the activities in the data set
Appropriately labeling the data set with descriptive activity names.
Creating a second, independent tidy data set with the average of each variable for each activity and each subject.
About R script

File with R code "run_analysis.R" perform 5 following steps (in accordance assigned task of course work)

Data

The Tidy.txt data file is a text file, containing space-separated values.

The first row contains the names of the variables, which are listed and described in the Variables section, and the following rows contain the values of these variables.

About variables:

featureNames -------> Assign data in features.txt

activityLabels -----> Assign data in activity_labels.txt

subjectTrain --------> Assign data in subject_train.txt

activityTrain -------->Assign data in y_train.txt

featuresTrain -------->Assign data in X_train.txt

subjectTest------->Assign data in subject_test.txt

activityTest------->Assign data in y_test.txt

featuresTest------->Assign data in X_test.txt

combine the respective data in training and test data sets corresponding to subject, activity and features.

*subject

*activity

*features

columnsWithMeanSTD ------->Extract the column indices that have either mean or std in them.

extractedData -------> selected columns in requiredColumns

tidyData -------> as a data set with average for each activity and subject
