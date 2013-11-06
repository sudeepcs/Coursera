corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  find_cor <- function(x){
    ifelse(sum(complete.cases(x)) == 0, 
           NA, 
           cor(x$sulfate, x$nitrate, "pairwise.complete.obs")
           )
  }
  
  # Get a list of all dataframes in the folder
  all_cc <- complete(paste0(directory, "/"))
  
  # Subset to get all file numbers that satisy threshold condition
  above_threshold <- as.vector(all_cc[all_cc$nobs > threshold, 1])
  
  # Reapply 
  ldf <- lapply(above_threshold, getmonitor, "specdata", FALSE)
  
  correlations <- sapply(ldf, find_cor)
  correlations
}