#This function takes a series of locally stored data sets and combines them in order 
#to obtain meaningful analysis. 


library(dplyr)
library(gsubfn)


#Create datasets with updated label

features <- read.table("UCi HAR Dataset/features.txt")
features_c <- as.character(features$V2)
train_x <- read.table("UCi HAR Dataset/train/X_train.txt", col.names = features_c)
test_x <- read.table("UCi HAR Dataset/test/X_test.txt", col.names = features_c)


#Read the Y_train.txt and convert to text variables

y_train <- read.table("UCi HAR Dataset/train/y_train.txt", colClasses = "character")
y_train_label <- gsubfn(".", list("1" = "WALKING", "2" = "WALKING_UPSTAIRS", "3" = "WALKING_DOWNSTAIRS", "4" = "SITTING", "5" = "STANDING",  "6" = "LAYING"), y_train$V1)

#Read subject id and bind train data sets

subject_train <- read.table("UCi HAR Dataset/train/subject_train.txt", colClasses = "numeric", col.names = "ID")
subject_train_2 <- mutate(subject_train, Cluster = "TRAIN")
train_set <- cbind(subject_train_2, y_train_label, train_x)
names(train_set) <- sub("y_train_label", "Activity", names(train_set))


#Read the Y_test.txt and convert to text variables

y_test <- read.table("UCi HAR Dataset/test/y_test.txt", colClasses = "character")
y_test_label <- gsubfn(".", list("1" = "WALKING", "2" = "WALKING_UPSTAIRS", "3" = "WALKING_DOWNSTAIRS", "4" = "SITTING", "5" = "STANDING",  "6" = "LAYING"), y_test$V1)


#Read subject id and bind test data sets

subject_test <- read.table("UCi HAR Dataset/test/subject_test.txt", colClasses = "numeric", col.names = "ID")
subject_test_2 <- mutate(subject_test, Cluster = "TEST")
test_set <- cbind(subject_test_2, y_test_label, test_x)
names(test_set) <- sub("y_test_label", "Activity", names(test_set))


#Create merged datasets and arrange by ID

project_set <- rbind.data.frame(train_set, test_set, stringsAsFactors = FALSE)
project_set <- arrange(project_set, project_set$ID)


#Extract a dataset with std and mean measurements

new_columnames <- c("ID", "Cluster", "Activity", features_c)
mean_col <- grep("-mean()", new_columnames, fixed = TRUE)
std_col  <- grep("-std()", new_columnames, fixed = TRUE)
filter_set <- select(project_set, 1:4, mean_col, std_col)


#A bit of love for the column names..

names(filter_set) <- gsub("tBodyAcc", "Time_Body_Acceleration", names(filter_set))
names(filter_set) <- gsub("tGravityAcc", "Time_Gravity_Acceleration", names(filter_set))
names(filter_set) <- gsub("tBodyAccJerk", "Time_Body_Acceleration_Jerk", names(filter_set))
names(filter_set) <- gsub("tBodyGyro", "Time_Body_Gyrometer", names(filter_set))
names(filter_set) <- gsub("fBodyGyro", "Frequency_Body_Gyrometer", names(filter_set))
names(filter_set) <- gsub("fBodyAcc", "Frequency_Body_Acceleration", names(filter_set))
names(filter_set) <- gsub("fBodyBody", "Frequency_Body_", names(filter_set))
names(filter_set) <- gsub("\\.\\.\\.", "()-", names(filter_set))
names(filter_set) <- gsub("\\.\\.", "()", names(filter_set))


#Creates a data set grouped by ID and Activity, containing the mean of all measurements

tidy_set <- filter_set %>% 
    
    group_by(ID, Activity) %>%
    summarise_at(vars(-Cluster), funs(mean))


#Export the requested data set

write.table(tidy_set, "project_set.txt", sep="\t", row.names = F)