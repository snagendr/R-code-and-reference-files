best <- function(state, outcome) {

## Read outcome data and ensure cols 11,17,23 are made numeric

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data[, 11] <- as.numeric(data[, 11])
data[, 17] <- as.numeric(data[, 17])
data[, 23] <- as.numeric(data[, 23])

## Check that state and outcome are valid

valid_outcome <- c("heart attack","heart failure","pneumonia")
outcome_col <- c(11,17,23) ## corresponding column numbers in csv for each of valid_outcome

valid_states  <-  unique(data$State)

if (!any(valid_outcome == outcome)) {
	stop ("invalid outcome")
}
if (!any(valid_states == state)) {
	stop ("invalid state")
}

## Return hospital name in that state with lowest 30-day death rate for any of the 3 outcomes

## first extract subset of the data for this state

data_state <- subset(data, data$State == state)

## Now find the best hospital in state - one with the lowest mortality rate for passed outcome value

outcome_index  <- match(outcome, valid_outcome) ## 1, 2 or 3

outcome_col_index <- outcome_col[outcome_index] ## 11, 17 or 23

lowest_value <- min(as.numeric(data_state[, outcome_col_index]), na.rm = TRUE)  

## Now subset state data to only rows with this minimum value

data_state1 <- subset(data_state, as.numeric(data_state[, outcome_col_index]) == lowest_value) 

## Now sort by hospital name

data_state2 <- data_state1[order(data_state1$Hospital.Name), ]

## Now print the first hospital in this sorted list (to resolve tie)

as.character(data_state2[1, 2])  ## data_state2[1=1st row, 2=hospital name column]

}