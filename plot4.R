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

par(mfrow=c(2,2),mar=c(4,4,2,1))

with(df2, {
    # Plot 1st graph
    plot(hpctime,g_a_power,xlab=" ",ylab="Global Active Power (Kilowatts)",type="n")
    lines(hpctime,g_a_power)

    # plot 2nd graph
    voltage <- as.numeric(voltage)
    plot(hpctime,voltage,xlab="datetime",type="n")
    lines(hpctime,voltage)

    # plot 3rd graph
    plot(hpctime,subm_1,type="n",xlab=" ",ylab="Energy sub metering",ylim=c(0,40))
    legend("topright",pch="_",col=c("black","red","blue"),legend=c("Sub-metering-1","Sub-metering-2","Sub-metering-3"),cex=0.5)  
    lines(hpctime,subm_1) # default col is black
    lines(hpctime,subm_2,col="red")
    lines(hpctime,subm_3,col="blue")
    
    #plot 4th graph
    g_r_power <- as.numeric(g_r_power)
    plot(hpctime,g_r_power,xlab="datetime",ylab="Global reactive power",ylim=c(0,0.5),type="n")
    lines(hpctime,g_r_power)
    })

# Now cleanup. Remove all downloaded files like "download_data", zipdir and all its contents

setwd(WDold) # Revert back to original working directory

closeAllConnections() # Close connections to file(s) opened, so they can be deleted

unlink("zipdir",recursive=TRUE)
unlink("download_data")    

# copy to png

dev.copy(png,file="plot4.png",width=480,height=480)

print("Job over")
print("Please review the plot4.png file created in your working directory")

dev.off()
    
