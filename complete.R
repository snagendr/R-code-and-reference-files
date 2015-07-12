complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

	path <- paste(getwd(),"/",directory,"/",sep="")
	filenames <- list.files(path)
	idx <- 0
	output <- matrix(nrow=332,ncol=2)
	
	for (i in id) {
		full_filename <- paste(path,filenames[i],sep="")
		data <- read.csv(full_filename)
		data1 <- na.omit(data)
		idx <- idx + 1
		output[idx,1] <- i
		output[idx,2] <- nrow(data1)
	}

	output_df <- data.frame(output)
	names(output_df) <- c("id","nobs")
	output_df1 <- na.omit(output_df)

output_df1
}