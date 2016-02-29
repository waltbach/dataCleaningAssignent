#
# run_analysis.R
#
#
# 
# Data Cleaning assignment
#
# Written by Walter Medenbach 29Feb 2016
#
#This program works with the "Human Activity Recognition Using Smartphones"
# Data Set which can be got from and is described at
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#
# It gets and cleans data as follows
#
# 1 Merges the training and the test sets to create one data set.
# 2 Extracts only the measurements on the mean and standard deviation for each
#   measurement.
# 3 Uses descriptive activity names to name the activities in the data set
# 4 Appropriately labels the data set with descriptive variable names.
# 5 From the data set in step 4, creates a second, independent tidy data set with
#   the average of each variable for each activity and each subject.
#
# The data frame from 5 is saved to the working direxctory with name
# 
# statsTidy.txt
#


# data source souce locations

actLabelsF    <- 'gs\\activity_labels.txt'
featuresF     <- 'gs\\features.txt'
 
testSubjectF  <- 'gs\\test\\subject_test.txt'
testActF      <- 'gs\\test\\y_test.txt'
testStatsF    <- 'gs\\test\\X_test.txt'

trainSubjectF <- 'gs\\train\\subject_train.txt'
trainActF     <- 'gs\\train\\y_train.txt'
trainStatsF   <- 'gs\\train\\X_train.txt'


#  download data

subjectTest    <- read.table(testSubjectF,col.names="subject")
activityTest   <- read.table(testActF,col.names="activityId")
statsTest      <- read.table(testStatsF)



subjectTrain    <- read.table(trainSubjectF,col.names="subject")
activityTrain   <- read.table(trainActF,col.names="activityId")
statsTrain      <- read.table(trainStatsF)

# create data frames

statsTest      <- bind_cols(subjectTest,activityTest,statsTest)
statsTrain      <- bind_cols(subjectTrain,activityTrain,statsTrain)

# 1. Merge the training and the test sets to create one data set.

stats <- bind_rows (statsTest,statsTrain)   

# 2. Extract only the measurements on
#    the mean and standard deviation for each measurement.

features <- read.table(featuresF)

#features <- gsub(",","",features$V2)

wantFeatures <- grep("mean\\(\\)|std\\(\\)",features$V2,ignore.case=TRUE) 

statsSub <- stats[,c(1,2,wantFeatures+2)]  


# 3. Uses descriptive activity names to name the activities in the data set

actLabel <- read.table(actLabelsF,col.names=(c("activityId","activity")))

statsSub <- merge(actLabel,statsSub,by.x="activityId",by.y="activityId")

statsSub$activityId <- NULL

# 4. Appropriately labels the data set with descriptive variable names. 

names(statsSub) <- c("activity","subject",as.character(features$V2[wantFeatures]))

# 5. From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.

statsMeans <- aggregate(. ~ activity + subject ,data=statsSub, mean)

#  lets tidy it more

df <- statsMeans
statsTidy <-  statsMeans %>%
  gather(key, value, -activity , -subject)
statsTidy$key <- sub("(.*)(-(mean|std).*)(-[XYZ])","\\1\\4\\2",statsTidy$key)
statsTidy$key <- gsub("[()]","",statsTidy$key);

  
statsTidy <- statsTidy %>%
  extract(key,c("sensor","stat"),"(.*)-(mean|std)$") %>%
  spread(stat, value)

names(statsTidy)[c(mean,std)] <- c("mean of mean","mean of std")

# export the table into a txt file

write.table(statsTidy, file = "statsTidy.txt")

