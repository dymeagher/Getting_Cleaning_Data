library(dplyr)
library(plyr)

#################
#################
#################

# 1. Combine data sets


if(!file.exists("./Class Project/data")) {dir.create("./Class Project/data")}

# Read in X_test data. No header, so header = F
# Read in X train and test data. Header default = F.
xTrain <- read.table("./Class Project/data/UCI HAR Dataset/train/x_train.txt")
xTest <- read.table("./Class Project/data/UCI HAR Dataset/test/x_test.txt")

# Read in y train and test data.
yTrain <- read.table("./Class Project/data/UCI HAR Dataset/train/y_train.txt")
yTest <- read.table("./Class Project/data/UCI HAR Dataset/test/y_test.txt")


# Read in subject data for train and test
subjectTrain <- read.table("./Class Project/data/UCI HAR Dataset/train/subject_train.txt")
subjectTest <- read.table("./Class Project/data/UCI HAR Dataset/test/subject_test.txt")

# Read in features, aka variable names for x_train & x_test
features = read.table("./class Project/data/UCI HAR Dataset/features.txt", header = F)

# Read in activit labels
activityLabels <- read.table("./class Project/data/UCI HAR Dataset/activity_labels.txt")

# Now combine the x, y and subject data respectively with rbind(). train first, test second.
xData <- rbind(xTrain, xTest)
yData <- rbind(yTrain, yTest)
subjectData <- rbind(subjectTrain, subjectTest)

# Rename columns in xData with features
colnames(xData) = features[,2]

# Rename columns in yData and subjectData
yData <- dplyr::rename(yData, activityCode = V1)
subjectData <- dplyr::rename(subjectData, subjectId = V1)

# Rename columns in activityLabels
colnames(activityLabels) = c("activityType", "activityCode")

# Bind the x, y and subject data together with cbind(). Order: subject, y, x
combinedData <- cbind(subjectData, yData, xData)

# Confirm that there are no column naming problems after cbind()
head(combinedData) [1:20]
dim(combinedData)


#################
#################
#################


# 2. Extract measurements on mean and SD

# Create variable name dataset to search for mean and sd varaibles later on
varNames <- variable.names(combinedData)

# Create logical vector of cols with mean or std in their names
stdMeanVars <- (grepl("activity..",varNames) | grepl("subject..",varNames) | 
                      grepl("-mean..",varNames) | grepl("-std..",varNames))

# Create a subset of combined data with just activity, subject, mean and std data
limitedCols <- combinedData[stdMeanVars == T]


#################
#################
#################

# 3. Uses descriptive activity names to name the activities in the data set
# (Helpful info for this: http://rprogramming.net/recode-data-in-r/)


limitedCols$activityCode[limitedCols$activityCode == 1] <- "WALKING"
limitedCols$activityCode[limitedCols$activityCode == 2] <- "WALKING_UPSTAIRS"
limitedCols$activityCode[limitedCols$activityCode == 3] <- "WALKING_DOWNSTAIRS"
limitedCols$activityCode[limitedCols$activityCode == 4] <- "SITTING"
limitedCols$activityCode[limitedCols$activityCode == 5] <- "STANDING"
limitedCols$activityCode[limitedCols$activityCode == 6] <- "LAYING"


#################
#################
#################

# 4. Appropriately labels the data set with descriptive variable names. 

# Loop to search for specific strings in column names, and replace them with 
  # more tidy versions.

names(limitedCols) <- gsub("-mean..", " Mean ", names(limitedCols))
names(limitedCols) <- gsub("-std\\(\\)", " Std Dev ", names(limitedCols))
names(limitedCols) <- gsub("()\\(\\)", " ", names(limitedCols))

tail(limitedCols)

#################
#################
#################

# 5. From the data set in step 4, creates a second, independent tidy data set 
  # with the average of each variable for each activity and each subject.
  
# colMeans info - https://docs.tibco.com/pub/enterprise-runtime-for-R/1.5.0_may_2013/TERR_1.5.0_LanguageRef/base/colMeans.html

limitedAvgs <- colMeans(limitedCols, na.rm = F)

# Write file
write.table(limitedAvgs, "limitedAvgs.txt", row.names = F)
