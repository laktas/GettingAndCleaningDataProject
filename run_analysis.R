##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## Heather Wade
## 2014-04-27

# runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################
library(reshape2)
# 1. Merge the training and the test sets to create one data set.

#Read Training Data Sets and Assign Variable Names:
features<-read.table('./data/features.txt',header=FALSE)
features[,2] <- as.character(features[,2])
activityType<-read.table('./data/activity_labels.txt',header=FALSE)
activityType[,2] <- as.character(activityType[,2])
subjectTrain<-read.table('./data/train/subject_train.txt',header=FALSE)
x_Train<-read.table('./data/train/x_train.txt',header=FALSE)
y_Train<-read.table('./data/train/y_train.txt',header=FALSE)

colnames(activityType)<-c("activityID","activityType")
colnames(subjectTrain)<-"subjectID"
colnames(x_Train)<-features[,2]
colnames(y_Train)<-"activityID"

#Binding data sets to form training data set
training<-cbind(y_Train,subjectTrain,x_Train)

#Read Test Data Sets and Assign Variable Names:
subjectTest<-read.table("./data/test/subject_test.txt", header=FALSE)
x_Test<-read.table("./data/test/x_test.txt", header=FALSE)
y_Test<-read.table("./data/test/y_test.txt", header=FALSE)

colnames(subjectTest)<-"subjectID"
colnames(x_Test)<-features[,2]
colnames(y_Test)<-"activityID"

#Binding data sets to form test data set
test<-cbind(y_Test,subjectTest,x_Test)

#Combining test and training data sets
data<-rbind(training,test)

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

mands_index<-grep(".*mean.*|.*std.*|.*activity.*|.*subject.*", colnames(data))
mands_names<-colnames(data)[mands_index]

data<-data[,mands_index]

# 3. Use descriptive activity names to name the activities in the data set

#Replace Activity ID with Activity Names in Data set

data<-merge(activityType,data,all=TRUE)

# 4. Appropriately label the data set with descriptive activity names. 

columns  = colnames(data); 

# Cleaning up the variable names
for (i in 1:length(columns)) 
{
      columns[i] = gsub("\\()","",columns[i])
      columns[i] = gsub("-std$","StdDev",columns[i])
      columns[i] = gsub("-mean","Mean",columns[i])
      columns[i] = gsub("^(t)","time",columns[i])
      columns[i] = gsub("^(f)","freq",columns[i])
      columns[i] = gsub("([Gg]ravity)","Gravity",columns[i])
      columns[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
      columns[i] = gsub("[Gg]yro","Gyro",columns[i])
      columns[i] = gsub("AccMag","AccMagnitude",columns[i])
      columns[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
      columns[i] = gsub("JerkMag","JerkMagnitude",columns[i])
      columns[i] = gsub("GyroMag","GyroMagnitude",columns[i])
}

colnames(data) = columns;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
for (j in 4:length(ncol(data))){
      data[,j]=as.numeric(data[,j])
}
data<-data[,!(names(data) %in% "activityType")]
data$activityID <- factor(data$activityID, levels = activityType[,1], labels = activityType[,2])
data$subjectID <- as.factor(data$subjectID)

data.melted <- melt(data, id = c("subjectID", "activityID"))
data.mean <- dcast(data.melted, subjectID + activityID ~ variable, mean)

write.table(data.mean, "tidy.txt", row.names = FALSE)
