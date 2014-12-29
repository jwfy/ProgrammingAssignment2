rankall <- function(outcome, num = "best")
{
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    id <- as.numeric()
    
    if(outcome == "heart attack")
        id <- 11
    else if(outcome == "heart failure")
        id <- 17
    else if(outcome == "pneumonia")
        id <- 23
    else 
        stop("invalid outcome")
    
    data <- data[,c(2, 7, id)]
    data[, 3] <- as.numeric(data[, 3])
    data <- data[complete.cases(data), ]
    # 上面就是筛选之后的数据
    data <- split(data, data$State)
    options(stringsAsFactors = FALSE)
    ans <- data.frame(stringsAsFactors=FALSE)
    for(item in data)
    {
        # item 现在就是每一个州的情况
        item <- item[order(item[3], item[1]), ]
        len <- as.numeric()
        lendata <- nrow(item)
        if(num == "best")
            len <- 1
        else if(num == "worst")
            len <- lendata
        else 
            len <- as.numeric(num)
        
        temans <- data.frame()
        temans <- data.frame(hospital = item[len, 1], state = item[1, 2], stringsAsFactors=FALSE)
        ans <- rbind(ans, temans)
    }
    rownames(ans) <- ans$state
    ans
}