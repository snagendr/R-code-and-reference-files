library(dplyr)

graphics.off() # clear any open graphs

# Download, unzip, and Read in the file from the url provided

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
WDold <- getwd()
setwd(WDold) # Ensure user is in current working directory

# download zip file to current working dir under name "download_data"
zipfile <- paste(WDold,"/download_data",sep="")

print("Downloading data file...")

# set options for downloading in windows environment and download url zipfile

setInternet2(TRUE)
download.file(url,zipfile,"internal",mode="wb")

#Now unzip it and store unzipped files in a subdirectory called "zipdir"

unzip(zipfile,exdir="zipdir")
# This unzips and creates the file under the folder WDold/zipdir

# Now set current working directory to be zipdir since that is where the unzipped file is

WDnew <- paste(WDold,"/zipdir",sep="")
setwd(WDnew)

# Now read the data file and process

print("Reading the data file. This may take 30 seconds .....")

df1 <- read.table("household_power_consumption.txt",sep=";",skip=1,stringsAsFactors = FALSE)
names(df1) <- c("hpcdate","hpctime","g_a_power","g_r_power","voltage","g_intensity","subm_1","subm_2","subm_3")

df2 <- filter(df1,hpcdate == "1/2/2007" | hpcdate == "2/2/2007") # df2 contains only data for these 2 dates


dt1 <- paste(df2$hpcdate,df2$hpctime) # convert time to proper date/time object using both date aand time columns
dt2 <- strptime(dt1,"%d/%m/%Y %H:%M:%S")

df2$hpctime <- dt2

df2$hpcdate <- as.Date(df2$hpcdate,"%d/%m/%Y") # convert date string to date object

df2$g_a_power <- as.numeric(df2$g_a_power)

hist(df2$g_a_power,col="red",main="Global Active Power",xlab="Global Active Power (Kilowatts)")

# Now cleanup. Remove all downloaded files like "download_data", zipdir and all its contents

setwd(WDold) # Revert back to original working directory

closeAllConnections() # Close connections to file(s) opened, so they can be deleted

unlink("zipdir",recursive=TRUE)
unlink("download_data")

# copy to png

dev.copy(png,file="plot1.png",width=480,height=480)

print("Job over")
print("Please review the plot1.png file created in your working directory")

dev.off()