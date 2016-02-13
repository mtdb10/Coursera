This file describes the variables, the data, and any transformations or work that I have performed to clean up the data.

The site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The run_analysis.R script performs the following steps to clean the data:
Read X_train.txt, y_train.txt and subject_train.txt from the "./UCI HAR Dataset/train" folder and store them in dataTrain, labelTrain and subjectTrain variables respectively.
Read X_test.txt, y_test.txt and subject_test.txt from the "./UCI HAR Dataset/test" folder and store them in dataTest, labelTest and subjectTest variables respectively.
Concatenate dataTest to dataTrain to generate a 10299x561 data frame, mergedData; concatenate labelTest to labelTrain to generate a 10299x1 data frame, mergedLabel; concatenate subjectTest to trainSubject to generate a 10299x1 data frame, mergedSubject.
Read the features.txt file from the "./UCI HAR Dataset" folder and store the data in a variable called features. We only extract the measurements on the mean and standard deviation. This results in a 66 indices list. We get a subset of mergedData with the 66 corresponding columns.
Clean the column names of the subset. We remove the "()" symbols in the names.
Read the activity_labels.txt file from the "./UCI HAR Dataset" folder and store the data in a variable called activity.
Clean the activity names in the second column of activity. We first make all names to lower cases. If the name has an underscore between letters, we remove the underscore.
Transform the values of mergedLabel according to the activity data frame.
Combine the mergedSubject, mergedLabel and mergedData by column to get a new cleaned 10299x68 data frame, finalData. Properly name the first two columns, "subject" and "activity". The "subject" column contains integers that range from 1 to 30 inclusive; the "activity" column contains 6 kinds of activity names; the last 66 columns contain measurements that range from -1 to 1 exclusive.
Write the finalData out to "final_data.txt" file in current working directory.
Finally, generate a second independent tidy data set with the average of each measurement for each activity and each subject. We have 30 unique subjects and 6 unique activities, which result in a 180 combinations of the two. Then, for each combination, we calculate the mean of each measurement with the corresponding combination. So, after initializing the result data frame and performing the two for-loops, we get a 180x68 data frame.
Write the result out to "independent.txt" file in current working directory.
