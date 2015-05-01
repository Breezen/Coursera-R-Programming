rankall <- function(outcome, num = "best") {
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
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    state <- sort(unique(data$State))
    hospital <- vector()
    i <- 0
    for (x in state) {
        i <- i + 1
        hospital[i] <- rankhospital(x, outcome, num)
    }
    res <- data.frame(hospital, state)
    row.names(res) <- state
    res
}