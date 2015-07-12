corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

	path <- paste(getwd(),"/",directory,"/",sep="")
	filenames <- list.files(path)
	output <- matrix(nrow=332,ncol=1)
	idx <- 0

	for (i in 1:length(filenames)) {
		full_filename <- paste(path,filenames[i],sep="")
		data <- read.csv(full_filename)
		data1 <- na.omit(data)
		if(nrow(data1) > threshold) {
			idx <- idx + 1
			output[idx] <- cor(data1$sulfate,data1$nitrate)
		}
	}
	if    (idx > 0) {
		 ## na.omit(output)does not seem to work 
		 ok  <- complete.cases(output)
		 output <- output[ok]

		 as.vector(output) }
	else {
		 zero_v <- vector(mode="numeric", length=0)
	}
}