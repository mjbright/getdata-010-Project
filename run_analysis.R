

# Install needed packages:
#
#install.packages("data.table")
#install.packages("reshape2")
#install.packages("knitr")

library(data.table)
library("reshape2")
library("knitr")

# Change to project directory:
setwd("D:/DATA/GettingCleaningData/Project")

##################################################################################
# 1. Merges the training and the test sets to create one data set.

# Read in subject training/test data:
dtTrain <- fread("Dataset/train/subject_train.txt")
dtTest  <- fread("Dataset/test/subject_test.txt" )

# Read in activity training/test data:

# Helper function to read in large data sets correctly:
readtable <- function (file) {
  dataf <- read.table(file)
  dt <- data.table(dataf)
  
  return(dt)
}

dtATrain <- readtable("Dataset/train/Y_train.txt")
dtATest <- readtable("Dataset/test/Y_test.txt")
dtTrain <- readtable("Dataset/train/X_train.txt")
dtTest <- readtable("Dataset/test/X_test.txt")

# Merge training and test datasets::
dtSubject <- rbind(dtTrain, dtTest)
dtActivity <- rbind(dtATrain, dtATest)

# Set column names:
setnames(dtSubject, "V1", "subject")
setnames(dtActivity, "V1", "activityNum")

dt <- rbind(dtTrain, dtTest)

dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

setkey(dt, subject, activityNum)

##################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

dtFeatures <- fread("Dataset/features.txt")
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]

##################################################################################
# 3. Uses descriptive activity names to name the activities in the data set

dtActivityNames <- fread("Dataset/activity_labels.txt")
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)

##################################################################################
# 4. Appropriately labels the data set with descriptive variable names. 

setkey(dt, subject, activityNum, activityName)

##################################################################################
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

grep_feature <- function (regex) {
  grepl(regex, dt$feature)
}

## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grep_feature("^t"), grep_feature("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grep_feature("Acc"), grep_feature("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grep_feature("BodyAcc"), grep_feature("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grep_feature("mean()"), grep_feature("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grep_feature("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grep_feature("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grep_feature("-X"), grep_feature("-Y"), grep_feature("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

##################################################################################
# Please upload the tidy data set created in step 5 of the instructions. Please upload your data set as a txt file created with write.table() using row.name=FALSE (do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).

write.table(dtTidy, "tidy.data.txt", row.name=FALSE)








