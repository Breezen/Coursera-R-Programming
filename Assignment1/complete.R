complete <- function(directory, id = 1:332) {
    y <- data.frame()

    for (i in id) {
        fname <- sprintf("%03d.csv", i)
        x <- read.csv(paste(directory, '/', fname, sep = ''))
        
        num <- 0
        for (j in 1:nrow(x)) {
            if (is.na(x[j, 2]) || is.na(x[j, 3])) next
            num <- num + 1
        }

        y <- rbind(y, list(id = i, nobs = num))
    }
    as.data.frame.matrix(y, row.names = as.character(1:length(id)))
}