# Project 
# 

#---------------------------------
# 1- Merges the training and the test sets to create one data set.
## Data has been previously downloaded in the working directory in ./UCI HAR Dataset/
#oad everything in data frame table
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", stringsAsFactors=FALSE)
Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt", stringsAsFactors=FALSE)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", stringsAsFactors=FALSE)
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", stringsAsFactors=FALSE)
Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt", stringsAsFactors=FALSE)
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", stringsAsFactors=FALSE)

library(dplyr)
#identify test source of data
d <- mutate(X_test, status = "test")
#combine data in same table instead of multiple source
d$activity <- Y_test$V1
d$subject <- subject_test$V1

#identify train source of data
dd <- mutate(X_train, status = "train")
#combine data in same table instead of multiple source
dd$activity <- Y_train$V1
dd$subject <- subject_train$V1

#here is the merged result
ddd <- bind_rows(d,dd)


#---------------------------------
# 2- Extracts only the measurements on the mean and standard deviation for each measurement.
#read data
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)
#find string with mean OR std
flist <- features[grepl("mean|std", features$V2),]
#then extract corresponding column including activity and subject
subdata <- ddd[,c(sapply(flist$V1, function(x) {sprintf("V%d",x)}), "activity", "subject")]

#---------------------------------
# 3- Uses descriptive activity names to name the activities in the data set
activities <-  read.table("UCI HAR Dataset/activity_labels.txt",stringsAsFactors=FALSE)
#using factor, convert activity ids to labels
subdata$activity <- as.factor(activities$V2[subdata$activity])
#subdata$activity <- factor(subdata$activity, labels = activities$V2)

#---------------------------------
# 4- Appropriately labels the data set with descriptive variable names.
colnames(subdata) <- c(sapply(flist$V2, function(x) {sprintf("%s",x)}), "activity", "subject")

#---------------------------------
# 5- From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.
# Please upload the tidy data set created in step 5 of the instructions. 
# Please upload your data set as a txt file created with write.table() using row.name=FALSE
gdata <- group_by(subdata, activity, subject)
tidydata <- summarise_each(gdata, funs(mean))
write.table(tidydata, "UCI HAR Dataset/NewDataset.txt", row.name=FALSE)

