count <- function(cause = NULL){
  
  require(stringr)
  
  if (is.null(cause)) stop("invalid option: please specify a cause")
  
  valid_cause <- c("asphyxiation", "blunt force", "shooting", "other", "stabbing", "unknown")
  if (sum(valid_cause == cause) == 0) stop("invalid option: please check the input cause")
  
  #
  homicides <- readLines("homicides.txt")
  
  causes <- sapply(homicides, function(x) tolower(str_match_all(x, "(?:Cause: )(.*?)(</dd>)")[[1]][2]))
  names(causes) <- NULL
  sum(causes == cause, na.rm = TRUE)
}