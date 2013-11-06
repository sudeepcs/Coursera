outcome <- read.csv("./outcome-of-care-measures.csv", colClasses = "character")

outcome[, 11] <- as.numeric(outcome[, 11])
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])

-- Subset to statates with at least  20 hospitals
keep.states <- names(table(outcome$State)[table(outcome$State) > 19])
outcome2 <- outcome[outcome$State %in% keep.states, ]