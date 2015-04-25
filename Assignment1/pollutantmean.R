pollutantmean <- function(directory, pollutant, id = 1:332) {
    y <- list()
    
    for (i in id) {
        monitor <- sprintf("%03d.csv", i)
        x <- read.csv(paste(directory, '/', monitor, sep = ''))
        y <- rbind(x, y)
    }
    
    if (pollutant == "sulfate") {
        return(mean(y[, 2], na.rm = TRUE))
    } else {
        return(mean(y[, 3], na.rm = TRUE)) 
    }
}