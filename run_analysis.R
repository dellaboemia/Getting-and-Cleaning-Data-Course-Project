library(dplyr)
library(data.table)
library(downloader)

# Create the run_analysis working directory, if it doesn't already exist, then download and unzip the dataset
setwd("C:\\Users\\John\\Documents\\Dropbox\\Online Learning\\Data Science\\Data Science Specialization\\Getting & Cleaning Data\\Code")
if (!file.exists("run_analysis")){
  dir.create("run_analysis")
  setwd("./run_analysis")
  url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download(url, dest="dataset.zip", mode="wb") 
  unzip ("dataset.zip")
  setwd("UCI HAR Dataset")
  
} else {
  setwd("run_analysis/UCI HAR Dataset")
}

# Read all files
features        <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt",stringsAsFactor=FALSE)
test_subjects   <- read.table("test/subject_test.txt")
test            <- read.table("test/X_test.txt")
test_activity   <- read.table("test/y_test.txt")
train_subjects  <- read.table("train/subject_train.txt")
train           <- read.table("train/X_train.txt")
train_activity  <- read.table("train/y_train.txt")

# Combine subject data from the test and training datasets, then assign a variable name
subjects <-rbind(test_subjects, train_subjects)
names(subjects) <- "subject"

# Combine activity data from the test and training datasets, then assign a variable name.
activities <- rbind(test_activity, train_activity)
names(activities) <- "activities"

# Combine measurement data and add features
measurements <- rbind(test, train)
names(measurements) <- tolower(features$V2)

# Select the 66 mean and standard deviation variables from measurement data
measurements <- measurements[,grepl("(mean\\(\\)|std\\(\\))", names(measurements))]

# create descriptive variable names for selected features
names(measurements) <- names(measurements) %>%
  sub("^t", "time-", .)   %>%
  sub("^f", "frequency-", .)   %>%
  gsub("\\.","",.)  %>%
  gsub("body", "body-", .) %>%
  gsub("gravity", "gravity-", .) %>%  
  gsub("acc", "accelerometer-", .) %>%  
  gsub("gyro", "gyroscope-", .) %>%
  gsub("mag", "magnitude-", .) %>%
  gsub("std\\(\\)", "standard-deviation", .) %>%
  gsub("mean\\(\\)", "mean", .) 
  
# Combine subject, activity and measurement data, then remove activity number (activities)
analysis <- cbind(subjects, activities, measurements)
analysis <- merge(analysis, activity_labels, by.x = "activities", by.y = "V1")
analysis$activities <- NULL


# Summarize data by subject and activity
analysis_summary <- analysis %>% group_by(subject, V2) %>% summarise_each(funs(mean))

# Rename activity label columns 
names(analysis)[names(analysis) == "V2"] <- "activity-labels"
names(analysis_summary)[names(analysis_summary) == "V2"] <- "activity-labels"

# Write files
write.table(analysis, file = "run_analysis.txt", row.names = FALSE)
write.table(analysis_summary, file = "run_analysis_summary.txt", row.names = FALSE)
