pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

	sum_s   <- 0
	count_s <- 0
	sum_n   <- 0
	count_n <- 0
	return_val <- 0

	path <- paste(getwd(),"/",directory,"/",sep="")
	filenames <- list.files(path)
	
	for (i in id) {
		full_filename <- paste(path,filenames[i],sep="")
		data <- read.csv(full_filename)
		
		s1 <- complete.cases(data$sulfate)
		s2 <- data$sulfate[s1]

		n1 <- complete.cases(data$nitrate)
		n2 <- data$nitrate[n1]

		sum_s   <- sum_s   + sum(s2)
		count_s <- count_s + length(s2)
		
		sum_n   <- sum_n + sum(n2)
		count_n <- count_n + length(n2)
	}
	if (pollutant == "sulfate") {
		mean_s <- sum_s/count_s
		return_val <- mean_s }
	else {
		 if (pollutant == "nitrate") {
			mean_n <- sum_n/count_n	
			return_val <- mean_n
		 }
	}
	
	return_val
}