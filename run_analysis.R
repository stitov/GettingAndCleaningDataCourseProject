#setwd("C:/GitHub/GettingAndCleaningDataCourseProject")

# special function for attach packages
attach.package = function (p) {
    if (!is.element (p, installed.packages () [ , 1]))
        install.packages (p, dep = TRUE)
    
    require (p, character.only = TRUE)
}

# attach packages
attach.package ("plyr")
attach.package ("dplyr")

# reading all data
test <- list.files ("UCI HAR Dataset/test", full.names = T, pattern = "txt")
test <- llply (test, read.table)
names (test) <- c ("subject","X","Y")

train <- list.files ("UCI HAR Dataset/train", full.names = T, pattern = "txt")
train <- llply (train, read.table)
names (train) <- c ("subject","X","Y")

features <- read.table ("UCI HAR Dataset/features.txt", stringsAsFactors = F)
activityLabels <- read.table ("UCI HAR Dataset/activity_labels.txt")

# making testset and trainset with needed variables
testset  <- data.frame (test$subject, join (test$Y, activityLabels), test$X)
trainset <- data.frame (train$subject, join (train$Y, activityLabels), train$X)

# create dataset and names it
dataset <- rbind (testset, trainset)
colnames (dataset) <- c ("subject", "activityId", "activityLabel", features [ , 2]) 

# grep the mean and std variables and group_by variables for step 5
index <- grep ("subject|activityId|activityLabel|mean\\(\\)|std\\(\\)", colnames (dataset))

# step 5. New tidy dataset 
tidyRes <- dataset [index] %>%
    group_by (subject, activityId, activityLabel) %>%
    summarise_each (funs (mean))

# save result
write.table (tidyRes, "tidyRes.txt", row.names = F)

write.table (unique (tidyRes$subject), "colnames.txt", quote = F, row.names = F)




