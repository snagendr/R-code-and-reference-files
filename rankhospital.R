rankhospital <- function(state, outcome, num="best") {

## Read outcome data and ensure cols 11,17,23 are made numeric

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Remove NA values

## data <- na.omit(data1)
## data <- data1[complete.cases(data1),]

data[, 11] <- as.numeric(data[, 11])
data[, 17] <- as.numeric(data[, 17])
data[, 23] <- as.numeric(data[, 23])

## Check that state and outcome are valid

valid_outcome <- c("heart attack","heart failure","pneumonia")
outcome_col <- c(11,17,23) ## corresponding column numbers in csv for each of valid_outcome

valid_states  <-  sort(unique(data$State))

if (!any(valid_outcome == outcome)) {
	stop ("invalid outcome")
}
if (!any(valid_states == state)) {
	stop ("invalid state")
}

## Return hospital name in state of rank <num> with lowest 30-day death rate for any of the 3 outcomes

## first extract subset of the data for this state

ds <- subset(data, data$State == state)

## Now find the column - one with the lowest mortality rate for passed outcome value

outcome_index  <- match(outcome, valid_outcome) ## 1, 2 or 3

outcome_col_index <- outcome_col[outcome_index] ## 11, 17 or 23

## now sort the state data by outcome column, then by Hospital name
## syntax: x[order(x, na.last = NA)]
ds1 <- ds[order(ds[, outcome_col_index], ds$Hospital.Name, na.last = NA), ]

nrows <- nrow(ds1) ## Gives number of rows in ds1, so "worst" means last (nrows) of this df

if (num == "best") {n <- 1} ## "best" means 1st element
else
if (num == "worst"){n <- nrows} ## "worst" means last row of ds1
else
   {n <- as.integer(num)}

if (n > nrows) {output_string <- NA} ## return NA if num > # of hospitals in the state
else {output_string <- as.character(ds1[n, 2])}  ## ds1[n=rank row, 2=hospital name column]

output_string
}