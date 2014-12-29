best <- function(state, outcome)
{
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    id <- as.numeric()
    stateName <- data[,7]
    if(is.na(match(state, stateName)))
        stop("invalid state")
    
    if(outcome == "heart attack")
        id <- 11
    else if(outcome == "heart failure")
        id <- 17
    else if(outcome == "pneumonia")
        id <- 23
    else 
        stop("invalid outcome")
    
    data <- data[data$State==state,c(2, id)]
    data[, 2] <- as.numeric(data[, 2])
    data <- data[complete.cases(data), ]
    data[order(data[2], data[1]), ][1, 1]
} 