agecount <- function(age = NULL){
  
  require(stringr)
  
  if (is.null(age)) stop("invalid option: please specify a cause")
  
  homicides <- readLines("homicides.txt")
  
  ages <- sapply(homicides, function(x) (str_match_all(x, "(\\d+)( years old)")[[1]][2]))
  names(ages) <- NULL
  sum(ages == age, na.rm = TRUE)
}