
#install.packages("data.table")
#install.packages("reshape2")

#install.packages("knitr")

library(data.table)
library("reshape2")
library("knitr")

setwd("D:/DATA/GettingCleaningData/Project")


dtTrain <- fread("Dataset/train/subject_train.txt")
dtTest  <- fread("Dataset/test/subject_test.txt" )

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
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtATrain, dtATest)
setnames(dtActivity, "V1", "activityNum")

dt <- rbind(dtTrain, dtTest)

dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

setkey(dt, subject, activityNum)

dtFeatures <- fread("Dataset/features.txt")
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]

dtActivityNames <- fread("Dataset/activity_labels.txt")
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)

setkey(dt, subject, activityNum, activityName)

dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

grepthis <- function (regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

write.table(dtTidy, "tidy.data.txt", row.name=FALSE)


knitr("makeCodebook.Rmd", output="codebook.md", encoding="ISO8859-1", quiet=TRUE)
markdownToHTML("codebook.md", "codebook.html")








