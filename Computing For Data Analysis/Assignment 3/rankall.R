rankall <- function(outcome, num = "best"){
  options(warn = -1)
  
  ## Read outcome data
  raw_data <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")
  
  states <- sort(unique(raw_data$State))
  
  ## Check that state and outcome are valid
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  outcome_column <- c(11, 17, 23)
  
  if (sum(valid_outcome == outcome) == 0) stop("invalid outcome")
  
  ## Set appropriate outcome column to 'numeric' datatype
  x <- outcome_column[which(valid_outcome == outcome)]
  raw_data[, x] <- as.numeric(raw_data[, x])
  
  ## Return hospital name in that state with lowest 30-day death rate
  aso <- raw_data[, c(2, 7, x)]
  names(aso) <- c("name", "state", "rate")
  aso <- aso[!is.na(aso$rate), ]
  aso <- aso[with(aso, order(state, rate, name)), ]
  
  get_hospital <- function(df, rank){
    if (rank == "best") rank = 1
    if (rank == "worst") rank = nrow(df)
    df[rank, 1]
  }
  
  laso <- split(aso, aso$state)
  options(warn = 1)
  
  z <- sapply(laso, get_hospital, num)
  data.frame(hospital = unname(z), state = names(z))
  
  
}