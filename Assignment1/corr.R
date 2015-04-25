corr <- function(directory, threshold = 0) {
    data <- complete(directory)
    monitor <- data$id[data$nobs > threshold]
    
    id <- 1
    res <- vector(mode = "numeric", length = length(monitor))
    for (i in monitor) {
        fname <- sprintf("%03d.csv", i)
        data <- read.csv(paste(directory, '/', fname, sep = ''))
        flag <- (!is.na(data$sulfate)) & (!is.na(data$nitrate))
        sul <- data$sulfate[flag]
        nit <- data$nitrate[flag]
        res[id] <- cor(sul, nit)
        id <- id + 1
    }
    res
}