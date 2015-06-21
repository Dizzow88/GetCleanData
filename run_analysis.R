## T. Adam Dow
## Coursera Data Science Specialization
## Getting and Cleaning Data
##
## Course Project Submission
## June 21, 2015
##
## ------------------------------------
##  
## Project Requirements
## -------------------- 
## 1. Merges the training and the test sets to create one data set
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Set working directory
setwd("~/R/Working")

# Clear environment
rm(list=ls())

# Verify directories and read in files
if(file.exists("./UCI HAR Dataset")){
  data.feat=read.table("./UCI HAR Dataset/features.txt",header=FALSE)
  data.actv=read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE)  
}
if(file.exists("./UCI HAR Dataset/train")){
  data.subj.trn=read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE)
  data.xtrn=read.table("./UCI HAR Dataset/train/x_train.txt", header=FALSE)
  data.ytrn=read.table("./UCI HAR Dataset/train/y_train.txt", header=FALSE)
}
if(file.exists("./UCI HAR Dataset/test")){
  data.subj.tst=read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE)
  data.xtst=read.table("./UCI HAR Dataset/test/x_test.txt", header=FALSE)
  data.ytst=read.table("./UCI HAR Dataset/test/y_test.txt", header=FALSE)
}

# Clean the train data (simple name assignments)
names(data.feat)=paste(c("Feat_ID","Feat_Type"))
names(data.actv)=paste(c("Actv_ID","Actv_Type"))
names(data.subj.trn)=paste(c("Subj_ID"))
names(data.ytrn)=paste(c("Actv_ID"))

# Clean the train data (complex name assignments)
names(data.xtrn)=data.feat[,2] #Take the 51 observations in data.feat and assign them as data.trn names

# Clean the train data (similar to train assignments)
names(data.subj.tst)=paste(c("Subj_ID"))
names(data.xtst)=data.feat[,2]
names(data.ytst)=paste(c("Actv_ID"))

# Merge the train and test datasets (by column)
data.TRN=cbind(data.ytrn,data.subj.trn,data.xtrn)
data.TST=cbind(data.ytst,data.subj.tst,data.xtst)

# Attach both train and test datasets (by row) and initialize the result vector
data.FINAL=rbind(data.TRN,data.TST)
Result.Vector=names(data.FINAL)

# Compile a logic vector containing only ID, Mean and Standard Deviation fields, use to subset the final data
Logic.Vector = (grepl("Actv..",Result.Vector) | grepl("Subj..",Result.Vector) | grepl("-mean..",Result.Vector) & !grepl("-meanFreq..",Result.Vector) & !grepl("mean..-",Result.Vector) | grepl("-std..",Result.Vector) & !grepl("-std()..-",Result.Vector))
data.FINAL = data.FINAL[Logic.Vector==TRUE];

## 3. Uses descriptive activity names to name the activities in the data set
## -------------------------------------------------------------------------

# Merge with activity names and re-initialize the Result Vector
data.FINAL = merge(data.FINAL,data.actv,by="Actv_ID",all.x=TRUE)
Result.Vector  = names(data.FINAL)

## 4. Appropriately labels the data set with descriptive variable names
## --------------------------------------------------------------------

# Step through the result vector and change all awkwardly named values
for (i in 1:length(Result.Vector)){
  Result.Vector[i] = gsub("\\()","",Result.Vector[i])
  Result.Vector[i] = gsub("-std$","StdDev",Result.Vector[i])
  Result.Vector[i] = gsub("-mean","Mean",Result.Vector[i])
  Result.Vector[i] = gsub("^(t)","Time",Result.Vector[i])
  Result.Vector[i] = gsub("^(f)","Freq",Result.Vector[i])
  Result.Vector[i] = gsub("([Gg]ravity)","Gravity",Result.Vector[i])
  Result.Vector[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",Result.Vector[i])
  Result.Vector[i] = gsub("[Gg]yro","Gyro",Result.Vector[i])
  Result.Vector[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMag",Result.Vector[i])
}

names(data.FINAL)=Result.Vector

## 5. Create a 2nd, independent tidy data set with the average of each variable for each activity and each subject
## ---------------------------------------------------------------------------------------------------------------

# Subset the data.FINAL to exclude activity type and create the tidy dataset with means
data.FINAL.noa=data.FINAL[,names(data.FINAL) != "Actv_Type"]
data.TIDY=aggregate(data.FINAL.noa[,names(data.FINAL.noa) != c("Actv_ID","Subj_ID")],by=list(Actv_ID=data.FINAL.noa$Actv_ID,Subj_ID = data.FINAL.noa$Subj_ID),mean)

# Merge with activity data and export the dataset to the core directory
data.TIDY=merge(data.TIDY,data.actv,by="Actv_ID",all.x=TRUE)
write.table(data.TIDY, './output_file.txt',row.names=TRUE,sep='\t')




