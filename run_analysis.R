run_analysis <- function() {
        labelTrain <- read.table("./UCI HAR Dataset/train/Y_train.txt")
        dataTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
        subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        
        labelTest <- read.table("./UCI HAR Dataset/test/Y_test.txt")
        dataTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
        subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        
        mergedData <- rbind(dataTrain, dataTest)
        mergedLabel <- rbind(labelTrain, labelTest)
        mergedsubject <- rbind(subjectTrain, subjectTest)
        
        features <- read.table("./UCI HAR Dataset/features.txt")
        requestVariables <- grep("mean\\(\\)|std\\(\\)", features[, 2])
        mergedData <- mergedData[ , requestVariables]
        
        names(mergedData) <- gsub("\\(\\)", "", features[requestVariables, 2])
        activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
        activity[, 2] <- gsub("_", "", activity[, 2])
        activityLabel <- activity[mergedLabel[, 1], 2]
        mergedLabel[,1] <- activityLabel
        names(mergedLabel) <- "activity"
        names(mergedsubject) <- "subject"
        
        finalData <- cbind(mergedsubject, mergedLabel, mergedData)
        write.table(finalData, "final_data.txt")
        
        subjectLen <- length(table(mergedsubject)) # 30
        activityLen <- dim(activity)[1] # 6
        columnLen <- dim(finalData)[2]
        independent <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
        independent <- as.data.frame(independent)
        colnames(independent) <- colnames(finalData)
        row <- 1
        for(i in 1:subjectLen) {
                     for(j in 1:activityLen) {
                                 independent[row, 1] <- sort(unique(mergedsubject)[, 1])[i]
                                 independent[row, 2] <- activity[j, 2]
                                 temp1 <- i == finalData$subject
                                 temp2 <- activity[j, 2] == finalData$activity
                                 independent[row, 3:columnLen] <- colMeans(finalData[temp1&temp2, 3:columnLen])
                                 row <- row + 1
                             }
        }
        write.table(independent, "independent.txt", row.names = FALSE)
       }
