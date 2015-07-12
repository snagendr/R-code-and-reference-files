rankall <- function(outcome, num="best") {

## Create empty data frame for output to which we append

df <- data.frame(hospital=NULL, state=NULL, stringsAsFactors = FALSE) 


## Read outcome data and ensure cols 11,17,23 are made numeric

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data[, 11] <- as.numeric(data[, 11])
data[, 17] <- as.numeric(data[, 17])
data[, 23] <- as.numeric(data[, 23])

## Check that outcome is valid

valid_outcome <- c("heart attack","heart failure","pneumonia")
outcome_col <- c(11,17,23) ## corresponding column numbers in csv for each of valid_outcome

valid_states  <-  sort(unique(data$State))

if (!any(valid_outcome == outcome)) {
	stop ("invalid outcome")
}

## Loop through each state

for (s in valid_states) {
	## first extract data for this state

	ds <- subset(data, data$State == s)

	## Now find the column - one with the lowest mortality rate for passed outcome value

	outcome_index  <- match(outcome, valid_outcome) ## 1, 2 or 3

	outcome_col_index <- outcome_col[outcome_index] ## 11, 17 or 23

	## now sort the state data by outcome column, then by Hospital name

	ds1 <- ds[order(ds[, outcome_col_index], ds$Hospital.Name), ]

	nrows <- nrow(ds1) ## Gives number of rows in ds1, so "worst" means last (nrows) of this df

	if (num == "best") {n <- 1}     ## "best" means 1st row 
	else
	if (num == "worst"){n <- nrows} ## "worst" means last row 
	else {
   	 n <- as.integer(num)
	 h <- as.character(ds1[n, 2])   ## ds1[n=rank row, 2=hospital name column]
      }
	if (n > nrows) {h <- NA} ## return NA if num > # of hospitals in the state
	else {h <- as.character(ds1[n, 2])}  ## ds1[n=rank row, 2=hospital name column]

	df <- rbind(df, data.frame(hospital = h, state = s))
}
df
}