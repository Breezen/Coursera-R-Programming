rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    ## Check that state and outcome are valid
    if (outcome == 'heart attack')
        y <- 11
    else if (outcome == 'heart failure')
        y <- 17
    else if (outcome == 'pneumonia')
        y <- 23
    else stop('invalid outcome')
    if (!(state %in% data$State))
        stop('invalid state')
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    stateData <- data[data$State == state, ]
    stateData <- stateData[stateData[, y] != 'Not Available', ]
    pos <- sort(as.numeric(stateData[, y]))
    if (num == 'best') {
        num <- 1
    } else if (num == 'worst') {
        num <- length(pos)
    } else if (num > length(pos))
        return(NA)
    res <- ''
    for (x in 1:length(pos)) {
        if (pos[num] == as.numeric(stateData[x, y]))
            if ((res == '') || (stateData[x, ]$Hospital.Name < res))
                res <- stateData[x, ]$Hospital.Name
    }
    res
}