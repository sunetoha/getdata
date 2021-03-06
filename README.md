---
title: "README"
output: html_document
---
This program is used to convert the raw data obtained from :
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
into a tidy data set. 
The program assumes that the folder resulting from unziping the primary data (UCI HAR Dataset) is located in the users home directory.

# Read the data files into R
```{r}
setwd("~/UCI HAR Dataset")
features <- read.table("features.txt")
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")
```
# Merge the test and training sets
Create a vector of expected columnnames
```{R}
column_names <- c("subject", "activity", as.character(features$V2))
```
Add subject and activity identifiers to the training set, with updated columnnames
```{R}
# bind the activities to the training set
train_set <- cbind(y_train$V1, x_train)
# bind the subjects to the training set
train_set <- cbind(subject_train$V1, train_set)
# change the columnnames of the training set
colnames(train_set) <- column_names
```
Add subject and activity identifiers to the test set, with updated columnnames 
```{R}
# bind the activities to the test set
test_set <- cbind(y_test$V1, x_test)
# bind the subjects to the test set
test_set <- cbind(subject_test$V1, test_set)
# change the columnnames of the test set
colnames(test_set) <- column_names
```
## Bind the training set and test set
```{R}
merged_set <- rbind(train_set, test_set)
```
Change the activity and subjects variables to factors
```{R}
# change the activities to labelled factors
merged_set$activity <- factor(merged_set$activity, 
                             labels=activity_labels$V2)
# change the subjects to labelled factors
merged_set$subject <- factor(merged_set$subject, 
                             labels=paste("subject", as.character(1:30)))
```

# Extract only the mean() and std() data
Only use those variables from the raw data that use the mean() or std() functions. The variables containing meanfrequency() functions, the weighted average of the frequency components, are avoided due to the requirement for the "(" character. \\\ is required to select for the "("" character.
```{R}
extracted_set <- merged_set[, sort(c(grep("mean\\(", colnames(merged_set)), 
                                     grep("std\\(", colnames(merged_set)), 
                                     1, 2))]
```

# Change uninformative columnnames to informative columnnames
Prepare a list of column names from the raw data "un_info_colnames", and assign each element a sequence of five phrases to expand the column name into something more understandable. To eventually paste each element together in "info_colnames".
```{R}
un_info_colnames <- colnames(extracted_set)
info_colnames <- character()
# colnames 1 and 2 need no expansion.
info_colnames[1] <- "subject"
info_colnames[2] <- "activity"
```
## Loop through the columnnames and expand to something more understandable. 
Break the terms into 5 elements using grepl, each to be expanded and pasted into the appropriate spot in "info_colnames"
```{R}
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
```
Clean the column names to remove spaces
```{R}
colnames(extracted_set) <- make.names(info_colnames) 
# change the subject labels back to integers 
extracted_set$subject <- as.numeric(extracted_set$subject)
```
# Prepare tidy datatset
Create a list of dataframes derived from the extracted set, where each element of the list is a dataframe limited to a particular subject.
```{R}
library(dplyr)
# split the dataframe into a list of dataframes, one element for each subject
subject_list <- split(extracted_set, extracted_set$subject)
```
## Use lapply to go through each element of the list.
Process each dataframe by applying the dplyr functions to  group by activity and use summarise_each to calculate the mean of each variable. 
```{R}
output <- lapply(subject_list, FUN=function(x){
        by_activity <- x %>% group_by(activity)
        return(by_activity %>% summarise_each(funs(mean)))})
```
Bind the list of data frames into one big dataframe
```{R}
tidy_data <- do.call(rbind, output)
```
# Write a table for tidy data
```{R}
write.table(tidy_data, file="tidy_data.txt", row.name=FALSE, sep="\t")
```