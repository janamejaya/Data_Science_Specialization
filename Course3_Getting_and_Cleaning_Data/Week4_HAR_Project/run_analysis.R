# Function to download the data file and unzip is
download_data <- function(){
  # The following lines come straight from the Course Lectures
  if(!file.exists("./data")){dir.create("./data")}
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL,destfile="./data/dataset.zip",method="curl")
  unzip("./data/dataset.zip",exdir="./data")
  # At this point, the data is contained in "./data/UCI HAR Dataset"
}

# Function to read in the list of 561 feature labels
read_feature_list <- function(features){
  feature_file <<- "./data/UCI HAR Dataset/features.txt"
  featuredf <- read.table(feature_file,stringsAsFactors = FALSE)
  features <<- featuredf$V2
  rm("featuredf")
}

# Function to assign pathname for training data input files
assign_input_filenames1 <- function(data_train, activity_train, subject_train){
  data_train <<- "./data/UCI HAR Dataset/train/X_train.txt"       # 7352 rows, 561 columns
  activity_train <<- "./data/UCI HAR Dataset/train/y_train.txt"       # 7352 rows, 1 column
  subject_train <<- "./data/UCI HAR Dataset/train/subject_train.txt" # 7352 rows, 1 column
}

# Function to assign pathname for test data input files
assign_input_filenames2 <- function(data_test, activity_test, subject_test){
  data_test <<- "./data/UCI HAR Dataset/test/X_test.txt"         # 2947 rows, 561 columns
  activity_test <<- "./data/UCI HAR Dataset/test/y_test.txt"         # 2947 rows, 1 column
  subject_test <<- "./data/UCI HAR Dataset/test/subject_test.txt"   # 2947 rows, 1 column
}

assemble_training_data <- function(data1, activity1, subject1, features){
  # Merge data1, activity1, and subject1 column-wise to construct full training dataset
  df <- read.table(data1)
  colnames(df) <- features
  act <- read.table(activity1)
  colnames(act) <-c("Activity")
  subj <- read.table(subject1)
  colnames(subj) <-c("Subject")
  
  tmp <- bind_cols(subj, act)
  df_train <- bind_cols(tmp, df)
  
  rm("df","act","subj")
  df_train
}

assemble_testing_data <- function(data2, activity2, subject2, features){
  # Merge data2, activity2, and subject2 column-wise to construct full testing dataset
  df <- read.table(data2)
  colnames(df) <- features
  act <- read.table(activity2)
  colnames(act) <-c("Activity")
  subj <- read.table(subject2)
  colnames(subj) <-c("Subject")
  
  tmp <- bind_cols(subj, act)
  df_test <- bind_cols(tmp, df)
  
  rm("df","act","subj")
  df_test
}

merge_train_test_data <- function(df_train, df_test){
  # Since most datasets are split into training and test datasets without changing the
  # order of the columns, it is assumed that our training and test datasets follow this
  # convention
  # Merge full training and full test datasets 
  df_merged <- rbind(df_train, df_test)
  
  # return the merged dataset
  df_merged
}

make_subset_data <- function(full_dataset){
  # Save the 1 and 2 columns as they contain the activity and subject information
  list0<-c(1,2)
  
  # Save all variables containing mean as a list
  list1<-grep("mean",names(full_dataset))
  
  # Save all variables containing std as a list
  list2<-grep("std",names(full_dataset))
  
  # merge list1 and list2, sort it, and then merge resultant list with list0
  sorted_list <- append(list0,sort(append(list1,list2)))
  
  # Select column names
  selected_columns <- names(full_dataset)[sorted_list]
  
  # Return a dataset corresponding to the selected columns (subsetting based on column names)
  shortlist_dataset <- full_dataset[selected_columns]
  
  return(shortlist_dataset)
}

transform_colnames <- function(current_colnames) {
  # Reformat column labels to make them more descriptive
  # To understand which changes may be appropriate, first see the
  # list of column names with use names(shortlist_dataset)
  # Several things stand out immediately. All names have the 
  # format : [t,f]Attribute-Descriptor where Attribute could be
  # BodyBodyGyroJerkMag and Descriptor is either mean() or std()
  # followed by a -, and either X or Y or Z
  # A number of transformations can be conceived
  # 1. mean always comes as mean() unless it is meanFreq which always comes with ()
  #    So, the () could be removed. Typically tidy data uses "." to separate names
  #    This suggests replacing () with "."
  # 2. Instead of using - as Attribute-Descriptor separator, "." could be used for
  #    compatibility with tidy data
  # 3. [t,f] at the beginning could be replaced with "Time." or "Frequency."
  # 4. It appears that some variables have BodyBody in them. This could be a typo
  #    and could be fixed by replacing BodyBody with Body. For all column names
  #    with Body in them, replace Body with "Body."
  # 5. Variables with "Gravity" in them can be made more readable if they were replaced
  #    with "Gravity."
  # 6. The attribute ACC corresponds to Accelerometer. One could replace with full form
  # 7. Gyro could be replaced with "Gyroscope"
  
  # Replace all occurances of BodyBody with Body
  new_colnames <- gsub("BodyBody","Body",current_colnames)
  
  # Replace all occurances of"()" with ""
  new_colnames <- gsub("\\(\\)","",new_colnames)
  
  # Replace all occurances of strings starting with t with Time. 
  new_colnames <- gsub("^t","Time.",new_colnames)
  
  # Replace all occurances of strings starting with f with Frequency.
  new_colnames <- gsub("^f","Frequency.",new_colnames)
  
  # Replace all occurances of Body with "Body."
  new_colnames <- gsub("Body","Body.",new_colnames)
  
  # Replace all occurances of Gravity with "Gravity."
  new_colnames <- gsub("Gravity","Gravity.",new_colnames)
  
  # Replace all occurances of "Acc" with "Accelerometer"
  new_colnames <- gsub("Acc","Accelerometer",new_colnames)
  
  # Replace all occurances of "Gyro" with "Gyroscope"
  new_colnames <- gsub("Gyro","Gyroscope",new_colnames)
  
  # Replace "Jerk" with ".Jerk"
  new_colnames <- gsub("Jerk",".Jerk",new_colnames)
  
  # Replace "Mag" with ".Mag"
  new_colnames <- gsub("Mag",".Magnitude",new_colnames)
  
  # Replace "-" with "."
  new_colnames <- gsub("-",".",new_colnames)
  
  return(new_colnames)
}

transform_colnames2 <- function(current_colnames){
  # Reformat column labels to make them more descriptive
  # To understand which changes may be appropriate, first see the
  # list of column names with use names(shortlist_dataset)
  # Several things stand out immediately. All names have the 
  # format : [t,f]Attribute-Descriptor where Attribute could be
  # BodyBodyGyroJerkMag and Descriptor is either mean() or std()
  # followed by a -, and either X or Y or Z
  # A number of transformations can be conceived
  # 1. mean always comes as mean() unless it is meanFreq which always comes with ()
  #    So, the () could be removed. Typically tidy data uses "." to separate names
  #    This suggests replacing () with "."
  # 2. Instead of using - as Attribute-Descriptor separator, "." could be used for
  #    compatibility with tidy data
  # 3. [t,f] at the beginning could be replaced with "Time." or "Frequency."
  # 4. It appears that some variables have BodyBody in them. This could be a typo
  #    and could be fixed by replacing BodyBody with Body. For all column names
  #    with Body in them, replace Body with "Body."
  # 5. Variables with "Gravity" in them can be made more readable if they were replaced
  #    with "Gravity."
  # 6. The attribute ACC corresponds to Accelerometer. One could replace with full form
  # 7. Gyro could be replaced with "Gyroscope"
  
  
  # Construct a vector, original_names, which contains the list of words
  # in the column names that will be changed. Construct another vector,
  # new_names, that contains the replacement words
  original_names <- c("BodyBody","\\(\\)","^t","^f","Body","Gravity","Acc","Gyro","Jerk","Mag","-")
  new_names <- c("Body","","Time.","Frequency.","Body.","Gravity.","Accelerometer","Gyroscope",".Jerk",".Magnitude",".")
  
  # Now loop over the list of original_words
  for (i in seq_along(original_names)){
    current_colnames <- gsub(original_names[i], new_names[i], current_colnames)
  }
  # Return the modified column names
  current_colnames
}

transform_colnames3 <- function(current_colnames){
  # Reformat column labels to make them more descriptive
  # To understand which changes may be appropriate, first see the
  # list of column names with use names(shortlist_dataset)
  # Several things stand out immediately. All names have the 
  # format : [t,f]Attribute-Descriptor where Attribute could be
  # BodyBodyGyroJerkMag and Descriptor is either mean() or std()
  # followed by a -, and either X or Y or Z
  # A number of transformations can be conceived
  # 1. mean always comes as mean() unless it is meanFreq which always comes with ()
  #    So, the () could be removed. Typically tidy data uses "." to separate names
  #    This suggests replacing () with "."
  # 2. Instead of using - as Attribute-Descriptor separator, "." could be used for
  #    compatibility with tidy data
  # 3. [t,f] at the beginning could be replaced with "Time." or "Frequency."
  # 4. It appears that some variables have BodyBody in them. This could be a typo
  #    and could be fixed by replacing BodyBody with Body. For all column names
  #    with Body in them, replace Body with "Body."
  # 5. Variables with "Gravity" in them can be made more readable if they were replaced
  #    with "Gravity."
  # 6. The attribute ACC corresponds to Accelerometer. One could replace with full form
  # 7. Gyro could be replaced with "Gyroscope"
  
  
  # Construct a vector, original_names, which contains the list of words
  # in the column names that will be changed. Construct another vector,
  # new_names, that contains the replacement words
  
  original_names <- c("BodyBody","\\(\\)","^t","^f", "BodyAccJerkMag", "BodyAccMag", "BodyAccJerk", "BodyAcc","BodyGyroJerkMag","BodyGyroMag","BodyGyroJerk", "BodyGyro","GravityAccMag","GravityAcc","-")
  new_names <- c("Body","","Time.Domain.","Frequency.Domain.","Magnitude.Body.Jerk", "Magnitude.Body.Acceleration", "Body.Jerk","Body.Acceleration","Magnitude.Body.Angular.Acceleration","Magnitude.Angular.Velocity","Body.Angular.Acceleration", "Body.Angular.Velocity","Magnitude.Gravity.Acceleration","Gravity.Acceleration",".")
  
  # Now loop over the list of original_words
  for (i in seq_along(original_names)){
    current_colnames <- gsub(original_names[i], new_names[i], current_colnames)
  }
  # Return the modified column names
  current_colnames
}

run_analysis <- function(){
  ## Step 0: Download and extract the data archive from website
  # This step precedes any instructions for the project
  download_data()
  
  ## Step 1: Read in training and test data set. 
  # There are several components that have to be assembled.
  # For each person for whom data was collected (i.e., the subject) [30 subjects]
  # there was 6 activities (specified in the file: activity_labels)
  # For each activity, there are 561 variables that come from time series analysis 
  # and data transformation.
  
  # Read in the variable names from the features.txt file
  read_feature_list(features)
  
  # All this data is split into two sets labeled train and test
  assign_input_filenames1(data_train, activity_train, subject_train)  
  assign_input_filenames2(data_test, activity_test, subject_test)
  
  # Construct the full training dataset
  df_train <- assemble_training_data(data_train, activity_train, subject_train, features)
  
  # Construct the full testing dataset
  df_test <- assemble_testing_data(data_test, activity_test, subject_test, features)
  
  # Step 1: Merge the train and test datasets to create a new dataset named full_dataset
  # Assemble the full dataset by merging training and testing dataset
  full_dataset <- merge_train_test_data(df_train, df_test)

  #rm(features)
  
  ## Step 2:
  # Create the shortlist based on columns whose names contain mean and std
  # No distinction has been made between column names that have mean() in
  # them and those which have meanFreq in them. MeanFreq is just the mean of
  # some frequency and could be interpreted as a mean value. However, it is
  # possible to remove all column headers that contain meanFreq by grepping
  # for mean\\(\\) instead of mean.
  shortlist_dataset <- make_subset_data(full_dataset)
  
  ## Step 3: Relabel the activities
  # The activity labels are numeric. i.e 1,2,3,4,5,6 
  # and the meaning of these numbers is specified in the file activity_levels.txt
  # One way to convert from numeric to descriptive is by invoking factors
  # First, convert column 2 to a factor from integer data.
  # Second, change factor to new levels
  shortlist_dataset[,2]=as.factor(shortlist_dataset[,2])
  newfac <- c("Walking","Walking up the stairs","Walking down the stairs","Sitting","Standing","Laying Down")
  levels(shortlist_dataset[,2])<-newfac
 
  ## Step 4: Change column labels, i.e, variable names, to make them more descriptive
  current_colnames <- colnames(shortlist_dataset)
  new_colnames <- transform_colnames3(current_colnames)
  colnames(shortlist_dataset)<-new_colnames
  
  ## Step 5: Required to find the average value for each variable given the activity and subject
  # This requires grouping data by activity and subject. Once grouped, the mean value for each
  # variable can be calculated for each combination of activity and subject.
  groupeddf <- group_by(shortlist_dataset, Activity, Subject)
  tidydf <- summarize_each(groupeddf, funs(mean))
  
  # Save the tidy data frame in the file tidydata.txt
  write.table(tidydf,"tidydata.txt",col.names=TRUE, row.names=FALSE)
  #shortlist_dataset
}