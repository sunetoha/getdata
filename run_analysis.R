## Read the data files into R
setwd("~/UCI HAR Dataset")
activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")

## merge the test and training sets
# create a vector of expected columnnames
column_names <- c("subject", "activity", as.character(features$V2))
# bind the activities to the training set
train_set <- cbind(y_train$V1, x_train)
# bind the subjects to the training set
train_set <- cbind(subject_train$V1, train_set)
# change the columnnames of the training set
colnames(train_set) <- column_names
# bind the activities to the test set
test_set <- cbind(y_test$V1, x_test)
# bind the subjects to the test set
test_set <- cbind(subject_test$V1, test_set)
# change the columnnames of the test set
colnames(test_set) <- column_names
# bind the training set and test set
merged_set <- rbind(train_set, test_set)
# change the activities to labelled factors
merged_set$activity <- factor(merged_set$activity, 
                             labels=activity_labels$V2)
# change the subjects to labelled factors
merged_set$subject <- factor(merged_set$subject, 
                             labels=paste("subject", as.character(1:30)))

## extract only the mean() and std() data
extracted_set <- merged_set[, sort(c(grep("mean\\(", colnames(merged_set)), 
                                     grep("std\\(", colnames(merged_set)), 
                                     1, 2))]

## change uninformative columnnames to informative columnnames
un_info_colnames <- colnames(extracted_set)
info_colnames <- character()
info_colnames[1] <- "subject"
info_colnames[2] <- "activity"
for (i in 3:68) {
        # set the first phrase of the columnname

        if (grepl("mean", un_info_colnames[i])){
                info_colnames[i] <- "mean of"
        }else if(grepl("std", un_info_colnames[i])){
                info_colnames[i] <- "std of"
        }
        # set the second phrase of the columnname
        if (grepl("X$", un_info_colnames[i])){
                info_colnames[i] <- paste(info_colnames[i], "x-dim")
        }else if (grepl("Y$", un_info_colnames[i])){
                info_colnames[i] <- paste(info_colnames[i], "y-dim")
        }else if (grepl("Z$", un_info_colnames[i])){
                info_colnames[i] <- paste(info_colnames[i], "z-dim")
        }else{
                info_colnames[i] <- paste(info_colnames[i], "non-dim")
        }
        # set the third phrase of the columnname
        if(grepl("JerkMag", un_info_colnames[i])){
                info_colnames[i] <- paste(info_colnames[i], "magnitude of first derivative of")
        }else if(grepl("Jerk", un_info_colnames[i])){
                info_colnames[i] <- paste(info_colnames[i], "first derivative of")
        }else if(grepl("Mag", un_info_colnames[i])){
                info_colnames[i] <- paste(info_colnames[i], "magnitude of")
        }
        # set the fourth phrase of the columnname
        if (grepl("^t", un_info_colnames[i])){
                info_colnames[i] <- paste(info_colnames[i], "time domain")
        }else{
                info_colnames[i] <- paste(info_colnames[i], "frequency domain")
        }
        # set the fifth phrase of the columnname
        if (grepl("^.BodyAcc", un_info_colnames[i])){
                info_colnames[i] <- paste(info_colnames[i], "linear acceleration")
        }else if (grepl("^.BodyGyro", un_info_colnames[i])){
                info_colnames[i] <- paste(info_colnames[i], "angular acceleration")
        }else if (grepl("^.Gravity", un_info_colnames[i])){
                info_colnames[i] <- paste(info_colnames[i], "gravitational acceleration")
        }else if (grepl("^.BodyBodyAcc", un_info_colnames[i])){
                info_colnames[i] <- paste(info_colnames[i], "linear acceleration")
        }else if (grepl("^.BodyBodyGyro", un_info_colnames[i])){
                info_colnames[i] <- paste(info_colnames[i], "angular acceleration")
        }
}
## clean the column names to remove spaces
colnames(extracted_set) <- make.names(info_colnames) 
# change the subject labels back to integers 
extracted_set$subject <- as.numeric(extracted_set$subject)

# prepare tidy datatset
library(dplyr)
# split the dataframe into a list of dataframes, one element for each subject
subject_list <- split(extracted_set, extracted_set$subject)
# use lapply to summarise each element of the list by activity and the function mean
output <- lapply(subject_list, FUN=function(x){
        by_activity <- x %>% group_by(activity)
        return(by_activity %>% summarise_each(funs(mean)))})
# bind the list of data frames into one bid dataframe
tidy_data <- do.call(rbind, output)

## write a table for tidy data
write.table(tidy_data, file="tidy_data.txt", row.name=FALSE, sep="\t")
