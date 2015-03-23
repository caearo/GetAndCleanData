# Script for Coursera course: Get and Clean Data.
# 2015-03-23
# This script is successfully tested on OS X 10.10.2.

## Download the data from UCI HAR Dataset and perform the following tasks:
# 1.Merge the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names.
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# Download data.
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile="./Samsung.zip", method="curl")
unzip("Samsung.zip", files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir ="./", unzip = "internal", setTimes = FALSE)

# Read raw data.====
xTrain = read.csv("./UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE) # training dataset/observations
yTrain = read.csv("./UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE) # training labels
subTrain = read.csv("./UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE) # training subjects from 1-30

xTest = read.csv("./UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE) # test dataset/observations
yTest = read.csv("./UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE) # test labels from 1-6
subTest = read.csv("./UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE) # test subjects from 1-30

activityLabels = read.csv("./UCI HAR Dataset/activity_labels.txt", sep="", stringsAsFactors=FALSE, header=FALSE) # download activity labels (6*2 matrix)
features = read.csv("./UCI HAR Dataset/features.txt", sep="", header=FALSE) # download features to replace original variable names

# Assign names.
colnames(activityLabels) = c('activityId','activityLabels');
colnames(subTrain)  = "subjectId";
colnames(xTrain) = features[,2]; 
colnames(yTrain) = "activityId";

# Merge trainnig data.
trainingData = cbind(yTrain,subTrain,xTrain);


# Read test data.====
subTest = read.csv("./UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE); 
xTest = read.csv("./UCI HAR Dataset/test/x_test.txt", sep="", header=FALSE); 
yTest = read.csv("./UCI HAR Dataset/test/y_test.txt", sep="", header=FALSE); 

# Assign names
colnames(subTest) = "subjectId";
colnames(xTest) = features[,2]; 
colnames(yTest) = "activityId";

# Merge Test data
testData = cbind(yTest,subTest,xTest);

# Combine the training data set and test data set.====
training_Test = rbind(trainingData,testData);


#########################
#2. Extract only the measurements on the mean and standard deviation for each measurement.
#########################

colNames  = colnames(training_Test);
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & 
                   !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & 
                   !grepl("-std()..-",colNames));
training_Test = training_Test[logicalVector==TRUE];



#########################
# 3. Use descriptive activity names to name the activities in the data set
#########################

# Combine training_Test data set with acitivityType table
training_Test = merge(training_Test,activityLabels,by='activityId',all.x=TRUE);

# Rename the colNames vector
colNames  = colnames(training_Test); 


#########################
## 4. Appropriately label the data set with descriptive activity names. 
#########################

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Rename the descriptive column names
colnames(training_Test) = colNames;

##########################
# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
##########################

# Create a new table, training_Test without the activityType column
training_Test_NoActivityType  <- training_Test[,names(training_Test) != 'activityType'];
## Final Data Set
tidyData <- aggregate(training_Test_NoActivityType[,names(training_Test_NoActivityType) != c('activityId','subjectId')],
                     by=list(activityId=training_Test_NoActivityType$activityId,subjectId = training_Test_NoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData <- merge(tidyData,activityLabels,by='activityId',all.x=TRUE);

### Export the tidyData set
write.table(tidyData, './tidyData.txt',row.names=F);
