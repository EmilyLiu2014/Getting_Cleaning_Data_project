library("dplyr")
## read files
varData <- read.table("./features.txt")

tnLable <- read.table("./train/y_train.txt")
tnLable <- rename(tnLable, Activity=V1 )
tnSubject <- read.table("./train/subject_train.txt")
tnSubject <- rename(tnSubject, PerformerID=V1)

ttLable <- read.table("./test/y_test.txt")
ttLable <- rename(ttLable, Activity=V1 )
ttSubject <- read.table("./test/subject_test.txt")
ttSubject <- rename(ttSubject, PerformerID=V1)

tnData <- read.table("./train/X_train.txt")
ttData <- read.table("./test/X_test.txt")

#######
## Q4: Appropriately labels the data set with descriptive variable names.
#######
for (i in 1 : length(varData[,1])) {
    colnames(tnData)[i] <- as.character(varData$V2[i])
}
# Rename ttData column name
for (i in 1 : length(varData[,1])) {
    colnames(ttData)[i] <- as.character(varData$V2[i])
}

#######
## Q1: Merges the training and the test sets to create one data set.
#######
# train data (7352 obs & 563 var)
trainData <- data.frame()
trainData <- cbind(tnData, tnSubject)
trainData <- cbind(trainData, tnLable)
# test data (2947 obs & 563 var)
testData <- data.frame()
testData <- cbind(ttData, ttSubject)
testData <- cbind(testData, ttLable)
# merge train & test data into one (10299 obs & 563 var)
HARdata <- data.frame()
HARdata <- rbind(trainData, testData)

########
## Q2: Extracts only the measurements on the mean and standard deviation for each measurement. 
########
mean_std_idx <- grep("mean", colnames(HARdata))
mean_std_idx <- append(mean_std_idx, grep("std", colnames(HARdata)))
mean_std_idx <- append(mean_std_idx, grep("PerformerID", colnames(HARdata)))
mean_std_idx <- sort(append(mean_std_idx, grep("Activity", colnames(HARdata))))
# mean_std_HARdata (10299 obs, 81 var.)
mean_std_HARdata <- HARdata[, mean_std_idx]

########
## Q3: Uses descriptive activity names to name the activities in the data set
########
# mean_std_HARdata (10299 obs, 82 var.)
for (i in 1 : length(mean_std_HARdata[,1])) {  
    mean_std_HARdata$DescrActivity[i] <- if (mean_std_HARdata$Activity[i] == 1) {
        "WALKING"
    } else if(mean_std_HARdata$Activity[i] == 2) {
        "WALKING_UPSTAIRS"
    } else if (mean_std_HARdata$Activity[i] == 3) {
        "WALKING_DOWNSTAIRS"
    } else if (mean_std_HARdata$Activity[i] == 4) {
        "SITTING"
    } else if (mean_std_HARdata$Activity[i] == 5) {
        "STANDING"
    } else if (mean_std_HARdata$Activity[i] == 6) {
        "LAYING"
    } else {
        "N/A"
    }
}

#######
## Q4: Appropriately labels the data set with descriptive variable names.
#######
## This step has been done during the data set merge (see it before Q1)

#######
## Q5:From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.
#######
resultdata<- aggregate(select(mean_std_HARdata,-(DescrActivity)),
                    by=list(Activity.Group=mean_std_HARdata$Activity, 
                            PerformerID.Group=mean_std_HARdata$PerformerID),
                    FUN=mean, na.rm=TRUE)

for (i in 1 : length(resultdata[,1])) {  
    resultdata$DescrActivity[i] <- if (resultdata$Activity[i] == 1) {
        "WALKING"
    } else if(resultdata$Activity[i] == 2) {
        "WALKING_UPSTAIRS"
    } else if (resultdata$Activity[i] == 3) {
        "WALKING_DOWNSTAIRS"
    } else if (resultdata$Activity[i] == 4) {
        "SITTING"
    } else if (resultdata$Activity[i] == 5) {
        "STANDING"
    } else if (resultdata$Activity[i] == 6) {
        "LAYING"
    } else {
        "N/A"
    }
}
resultdata <- select(resultdata, -(PerformerID:Activity))
resultdata <- rename(resultdata, SubjectID=PerformerID.Group, Activity=Activity.Group)

#######
# resultdata (180 obs. and 82 vars)
########
