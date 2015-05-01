best <- function(state, outcome) {
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
    
    ## Return hospital name in that state with lowest 30-day death rate
    min <- 100;
    for (x in 1:nrow(data)) {
        if (data[x, ]$State == state) {
            if (data[x, y] == 'Not Available') next
            if (as.numeric(data[x, y]) < min) {
                min <- as.numeric(data[x, y])
                res <- data[x, ]$Hospital.Name
            } else if (as.numeric(data[x, y]) == min)
                if (data[x, ]$Hospital.Name < res)
                    res <- data[x, ]$Hospital.Name
        }
    }
    res
}
