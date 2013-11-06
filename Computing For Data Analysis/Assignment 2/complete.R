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
  
  # Construct the filepath
  
  returnCompleteCases <- function(df){
    cc <- sum(complete.cases(df))
    cc
  }
  
  # Create a list of dataframes by using the getmonitor function
  # to read in all the csv files specified by the user
  ldf <- lapply(id, getmonitor, "specdata", FALSE)
  
  # Apply the returnCompleteCases function and simplify the result
  nobs <- sapply(ldf, returnCompleteCases)

  # Assemble the result
  result <- data.frame(id, nobs)   
  result
}