pollutantmean <- function(directory, pollutant, id = 1:332) {
    sum <- 0
    num <- 0
    col = if (pollutant == 'sulfate') {2} else {3}
    
    for (i in id) {
        monitor <- sprintf("%03d.csv", i)
        x <- read.csv(paste(directory, '/', monitor, sep = ''))
        
        for (j in 1:nrow(x)) {
            if (is.na(x[j, col])) next
            sum <- sum + x[j, col]
            num <- num + 1
        }
    }
    sum / num
}