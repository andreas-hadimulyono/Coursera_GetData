## using dplyr and reshape2 library

library(dplyr)
library(reshape2)

## ASSUMPTION
## Following files are already in the working directory
## x_test.txt, x_train.txt, features.txt, activity_labels.txt, y_test.txt, y_train.txt

run_analysis <- function(){
    x_test_raw <- read.table(header=FALSE, file="X_test.txt")
    x_train_raw <- read.table(header=FALSE, file="X_train.txt")
    
    ## step 1: merge the two data sets
    x_merged_raw <- rbind(x_test_raw, x_train_raw)
    
    ## step 2: get all mean and std deviation
    ## the columns are marked with "xx-mean()" and "xx-std()" names in features.txt for this exercise
    features_raw <- read.table(header=FALSE, file="features.txt")
    allMean <- grep("mean()", features_raw[,2])
    allStd <- grep("std()", features_raw[,2])
    
    ## combine all the desired columns, sort it after merge
    desiredColumns <- sort(c(allMean, allStd))
    
    ## column names, just in case they're needed
    ## since the first col of the features.txt is also line number, we can just pass it to 
    ## the features_raw
    columnNames <- features_raw[desiredColumns,2]
    
    ## filter the raw data
    x_merged_filtered <- x_merged_raw[,desiredColumns]
    
    ## step 3: applying activity labels
    y_test_raw <- read.table(header=FALSE, file="y_test.txt")
    y_train_raw <- read.table(header=FALSE, file="y_train.txt")
    y_merged <- rbind(y_test_raw, y_train_raw)
    
    ## activity reference
    activity_reference <- read.table(header=FALSE, file="activity_labels.txt")
    
    ## join, resulting in 2 column data frame, "V1" is the default column name
    y_labeled <- left_join(y_merged,activity_raw,by="V1")
    
    ## step 4
    ## rename column names to activity labels
    y_labeled <- rename(y_labeled, activity_id = V1, activity_label = V2)
    
    ## rename the column names for all the data
    ## we already have column names, ordered based on the order they appear in features.txt
    ## the raw data also should be selected based on that same order
    ## so we can just apply the columnNames to the data frame
    colnames(x_merged_filtered) <- columnNames
    
    ## step 5
    ## get the subjects
    subject_test <- read.table(header=FALSE, file="subject_test.txt")
    subject_train <- read.table(header=FALSE, file="subject_train.txt")
    subject_merged <- rbind(subject_test, subject_train)
    
    ## append the subject and activity into dataset
    staging_data_set <- cbind(staging, subject_id = subject_merged[,1], activity_label = y_labeled[,2])
    
    ## melt the data set
    melt_data_set <- melt(staging_data_set, id=c("subject_id","activity_label"))
    
    ## set the grouping
    grouped_data_set <- group_by(melt_data_set, subject_id, activity_label)
    
    ## summarize
    result <- summarize(result_test, averageValue = mean(value))
    
    ## write output file
    write.table(result, "output_result.txt", row.names=FALSE)
    
    return(result)
}