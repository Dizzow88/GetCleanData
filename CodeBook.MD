# CodeBook for Course Project

T. Adam Dow

Coursera Data Science Specialization

Getting and Cleaning Data

Course Project Submission

June 21, 2015

## Reading Data
Data is read from the root file provided into the following data frames:

data.feat: features.txt

data.actv: activity_labels.txt

data.subj.trn: subject_train.txt

data.xtrn: x_train.txt

data.ytrn: y_train.txt

data.subj.tst subject_test.txt

data.xtst x_test.txt

data.ytst: y_test.txt


### 1. Merges the training and the test sets to create one data set
data.TRN: Column bind of data.ytrn,data.subj.trn,data.xtrn
data.TST: Column bind of data.ytst,data.subj.tst,data.xtst
data.FINAL=data.TRN+data.TST

### 2. Extracts only the measurements on the mean and standard deviation for each measurement
Logic.Vector is a boolean vector compiled with the RegEx grepl function on key fields
data.FINAL is reset based on Logic Vector

### 3. Uses descriptive activity names to name the activities in the data set
data.FINAL is merged with the data.actv data frame using the merge command

### 4. Appropriately labels the data set with descriptive variable names
The names of the data.FINAL data frame are iterated through, replacing confusing text

### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Activity type is subsetted out followed by aggregation and merge
