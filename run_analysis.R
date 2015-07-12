# set library to use

require(dplyr)

# Download, unzip, and Read in the various files

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Get the current working directory and download zip file there under name "Project Data"

WDold <- getwd()

setwd(WDold)

zipfile <- paste(WDold,"/Project Data",sep="")

# set options for downloading in windows environment

print("Downloading and unzipping files - this may take 30 seconds ....................")

setInternet2(TRUE)

download.file(url,zipfile,"internal",mode="wb")

#Now unzip it and store unzipped files in a subdirectory called "zipdir"

unzip(zipfile,exdir="zipdir")

# This unzips several files and creates them under the folder WDold/zipdir/UCI HAR Dataset

WDnew <- paste(WDold,"/zipdir/UCI HAR Dataset",sep="")

# We will make WDnew the new working directory, and revert back to WD old when the program ends
# we will also remove "zipdir" and all downloaded contents when program ends

setwd(WDnew)

print("Reading & processing files")

#---------------------------------------------------------------------------------------------------------
# read features.txt into a df calle fdf - this contains 2 columns: column number, and column name

fdf <- read.table("features.txt")

names(fdf) <- c("colnumber", "colname")     # Give meaningful column names instead of V1 and V2

#filter out all column names containing "mean" or "std" in column V2 and store in a new df called fdf1.

fdf1 <- fdf[grepl("mean|std", fdf$colname),]

# Now remove "()" in some of the names to make them valid names, as follows:

fdf1$colname <- gsub("[^0-9A-Za-z-]", "", fdf1$colname)

# Now convert any uppercase to lowercase (this is to conform to proper variable naming standards 
# as outlined in Week 4 video lecture 1 slide 16 ("Editing test variabes")

fdf1$colname <- tolower(fdf1$colname)

# Now extract only the column numbers from fdf1 and store in a vector called c - we will use c shortly

c <- fdf1$colnumber 

#---------------------------------------------------------------------------------------------------------

# read the activity table

activitydf <- read.table("activity_labels.txt",stringsAsFactors=FALSE)    # This file has 6 rows and 2 columns

names(activitydf) <- c("actnumber","actname")

#---------------------------------------------------------------------------------------------------------

# Read Y_train.txt

ytraindf = read.table("train/y_train.txt")     # This file has 7352 ros and only 1 column - activity number

names(ytraindf) <- "activity"     # We give a proper column name ("activity") instead of calling it V1

#---------------------------------------------------------------------------------------------------------

# Read Subject_train.txt

straindf = read.table("train/subject_train.txt")     # This file has 7352 rows and only 1 column - subject id number

names(straindf) <- "subject"      # We give a proper column name ("Subject") instead of calling it V1

# do head(straindf) and paste here
#---------------------------------------------------------------------------------------------------------


# Read X_train.txt

xtraindf = read.table("train/x_train.txt")


# This file has 561 columns, and we need to extract only the columns having "mean" or "std" in the column name
# To achieve this we subset xtraindf into a new df called xtdf having only mean and std columns

xtraindf1 <- xtraindf[,c]      # we are using the vector c we created above to subset

# Now we have reduced columns from 561 to 79

# The columns of xtraindf1 are V1, V2 etc. We need to name them properly.
# Therefore we will replace V1, V2 etc. with proper names from the fdf1 df that we created and edited above 

names(xtraindf1) <- fdf1$colname

# Now we do the following:
# cbind xtraindf1 with activities df (called ytraindf above) which also has 7352 rows (and just 1 column). 
# Also replace activity code by descriptive name as requested in step 3 of instructions

# cbind with subject train df (called straindf above) which also has 7352 rows (and just 1 column) 

# Doing so will create 1 tidy df having mean(), std(), activity, subject in each xtrain row
# This will satisfy Hadley Wickham's requirement of tidy data (each variable is a column, each observation is a row)
# Also refer to David (TA)'s posts and specifications on what he believes is "tidy data" fot this assignment:
# Paste links here

xtraindf2 <- cbind(xtraindf1,ytraindf,straindf) # cbind activity(from ytraindf), and Subject(from straindf) 

# Now update the "activity" column from a number to a descriptive value like (Walking, Sitting etc.) 

xtraindf3 <- mutate(xtraindf2,activity = activitydf$actname[match(activity,activitydf$actnumber)])


# We are done with the training data preparation: xtraindf3 is our final tidy training df

# Now we will repeat the same above steps for test data to create another tidy df for test data (where traindata=FALSE)

# Then we will rbind training and test data df to create the final df


# Read Y_test.txt

ytestdf = read.table("test/y_test.txt")     # This file has 2947 rows and only 1 column - activity number

names(ytestdf) <- "activity"     # We give a proper column name ("activity") instead of calling it V1

#---------------------------------------------------------------------------------------------------------

# Read Subject_test.txt

stestdf = read.table("test/subject_test.txt")     # This file has 2947 rows and only 1 column - subject id number

names(stestdf) <- "subject"      # We give a proper column name ("Subject") instead of calling it V1

#---------------------------------------------------------------------------------------------------------


# Read X_test.txt

xtestdf = read.table("test/x_test.txt")

# This file has 2947 rows & 561 columns, and we need to extract only the columns having "mean" or "std" in the column name
# To achieve this we subset xtestdf into a new df called xtestdf1 having only mean and std columns

xtestdf1 <- xtestdf[,c]      # we are using the vector c we created above to subset

# we have reduced columns from 561 to 79

# The columns of xtestdf1 are V1, V2 etc. We need to name them properly.
# Therefore we will replace V1, V2 etc. with proper names from the fdf1 df that we created and edited above 

names(xtestdf1) <- fdf1$colname

# Now we do the following:
# cbind xtestdf1 with activities df (called ytestdf above) 
# Also replace activity code by descriptive name as requested in step 3

# cbind with subject test df (called stestdf above) 

# Doing so will create 1 tidy df having mean(), std(), activity, subject in each xtestdf1 row, just like train data


xtestdf2 <- cbind(xtestdf1,ytestdf,stestdf) # cbind activity(from ytestdf), and Subject(from stestdf) 

# Now mutate the "activity" column from a number to a descriptive value like (Walking, Sitting etc.) 

xtestdf3 <- mutate(xtestdf2,activity = activitydf$actname[match(activity,activitydf$actnumber)])

# We are done with the test data preparation: xtestdf3 is our final tidy test df

# Now we will rbind training and test data df to create the final df

combinedf <- rbind(xtraindf3,xtestdf3)

# Now create a second, independent tidy data set 
# with the average of each variable for each activity and each subject

# First use group_by to create a grouped df
#

by_a_s_df <- group_by(combinedf,activity,subject)

# Now apply summarize_each function to get mean for all variables

mean_by_a_s_df <- summarise_each(by_a_s_df, funs(mean)) # This df has 180 rows and 81 columns

# Now prefix "m." before each column name of mean_by_a_s_df in order to show that 
#each column here is a mean calculation variable, and thus distinguish from similar column names in other dfs

names(mean_by_a_s_df) <- paste0("m.",names(mean_by_a_s_df))

# Now write this out as a txt file created with write.table() using row.name=FALSE (as per instructions)

print ("Creating output summary file")

# Now that files are all processed, revert back to original working directory

setwd(WDold)     

# write output file

write.table(mean_by_a_s_df,file="mean_by_activity_subject.txt", row.names = FALSE)

print ("Removing zip files and temp directory")

# Now remove all downloaded files like "Project Data", zipdir and all its contents

unlink("zipdir",recursive=TRUE)

unlink("Project Data")

print("Job over")

print("Please review and then delete the output file created in your woking directory")
print(" ")
print("To view the output summary file created, please run the following two commands:")
print("1) data <- read.table('mean_by_activity_subject.txt',header=TRUE)")
print("2) View(data)")
print ("***** Thank you *****")


