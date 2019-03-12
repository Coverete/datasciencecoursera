complete <- function(directory, id = 1:332){
    nobs <- c()
    ids <- c()
    files <- list.files(directory)
    for (index in id) {
        data <- read.csv(paste(directory, "/", files[index], sep=""))
        nobs <- c(nobs, sum(complete.cases(data)))
        ids <- c(ids, index)
    }
    data.frame(id = ids, nobs = nobs)
}