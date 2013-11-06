rankhospital <- function(state, outcome, num = "best") {
  options(warn = -1)
  
  ## Read outcome data
  raw_data <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  valid_state <- unique(raw_data$State)
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  outcome_column <- c(11, 17, 23)
  
  if (sum(valid_state == state) == 0) stop("invalid state")
  if (sum(valid_outcome == outcome) == 0) stop("invalid outcome")
  
  
  ## Set appropriate outcome column to 'numeric' datatype
  x <- outcome_column[which(valid_outcome == outcome)]
  raw_data[, x] <- as.numeric(raw_data[, x])
  
  ## Return hospital name in that state with lowest 30-day death rate
  state_outcome <- raw_data[raw_data$State == state, c(2, x)]
  state_outcome <- state_outcome[!is.na(state_outcome[, 2]), ]
  names(state_outcome) <- c("name", "rate")
  state_outcome <- state_outcome[with(state_outcome, order(rate, name)), ]
  
  options(warn = 1)
  if (num == "best") num = 1
  if (num == "worst") num = nrow(state_outcome)
  
  state_outcome[num, 1]
}