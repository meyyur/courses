##Merges the training and the test sets to create one data set.
##Extracts only the measurements on the mean and standard deviation for each measurement.
##Uses descriptive activity names to name the activities in the data set
##Appropriately labels the data set with descriptive variable names.
##From the data set in step 4, creates a second, independent tidy data set with the average of each
## variable for each activity and each subject.
## http://archive.ics.uci.edu/ml/datasets/Smartphone-Based+Recognition+of+Human+Activities+and+Postural+Transitions
##04/23/2016 CM USA
Run_Analysis <- function() {

  
  ##source("run_analysis.R")
  ##Run_Analysis()
  
  ##'activity_labels.txt': Links the class labels with their activity name.
  
  ##'train/X_train.txt': Training set.
  
  ##train/y_train.txt': Training labels.
  
  ##test/X_test.txt': Test set.
  
  ##'test/y_test.txt': Test labels.
  ##'
  ## Inertial Sensor Data and labels of all activities performed.

  
  rm(list=ls()) #will remove ALL objects 
  setwd("c:/coursera")
  library(plyr)
  library(data.table)

  
  ## Human Volunteers(subjects) performed the activities
  SubjectTrainData <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
  SubjectTestData <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
  Volunteers <- rbind(SubjectTestData,  SubjectTrainData)
  colnames(Volunteers) = "Volunteer_ID"
  
  ##Ids and labels of all activities performed.
 
  YTestData <- read.table("./data/UCI HAR Dataset/test/y_test.txt") ##Test labels.
  YTrainData <- read.table("./data/UCI HAR Dataset/train/y_train.txt") ##Training labels.
  ActivityID <- rbind(YTestData,  YTrainData)
  ActivityNames <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
  Activities <- join(ActivityID,ActivityNames)
  Activity_Name <- Activities[,2]
  Activity <-data.frame(Activity_Name)
  colnames(Activity) <-"Activity_Name"
  
  ##features_info.txt': Shows information about the variables used on the feature vector.
  ##features.txt': List of all features.
  

  Features <- read.table("./data/UCI HAR Dataset/features.txt")
 
  Selected_Features <-intersect(grep("-mean()|-std()",Features[,2]),grep("-meanFreq()",Features[,2],invert=TRUE))

  Features[,2] <- gsub("-","_",Features[,2],)
  
  Features[,2] <- sub("mean","Mean",Features[,2],)
  
  Features[,2] <- sub("std","Std",Features[,2],)
  
  Features[,2] <- sub("\\(\\)","",Features[,2],)
  
  ## Inertial Sensor Data
  
  XTestData <- read.table("./data/UCI HAR Dataset/test/X_test.txt") ##Test set.
  XTrainData <- read.table("./data/UCI HAR Dataset/train/X_train.txt") ##Training set
  SensorMeasurements <-rbind(XTestData,XTrainData) 
  SelectedMeasurements <- SensorMeasurements[,Selected_Features]
  names(SelectedMeasurements)<-Features$V2[Selected_Features]
  ##write.table(SelectedMeasurements,"smdata.txt")

  MergedData <- cbind(Volunteers,Activity,SelectedMeasurements)
  write.table(MergedData,"mergeddata.txt",row.names=FALSE)
  
 
  
  MergedData <-data.table(MergedData)
  
  TidyData <- MergedData[,lapply(.SD,mean),by=c("Volunteer_ID","Activity_Name")]
  

  write.table(TidyData,"tidydata.txt",row.names=FALSE)
 
 
  
  rm(list=ls()) #will remove ALL objects 

}