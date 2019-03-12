source("complete.R")

corr <- function(directory, threshold = 0){
    complete_obsrv <- complete(directory)
    above_threshold <- complete_obsrv$id[complete_obsrv$nobs > threshold]
    files <- list.files(directory)
    corr_v <- c()
    for (index in above_threshold) {
        data <- read.csv(paste(directory, "/", files[index], sep=""))
        corr_v <- c(corr_v, cor(data$sulfate, data$nitrate, use="complete.obs"))
    }
    corr_v
}