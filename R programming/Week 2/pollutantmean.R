pollutantmean <- function(directory, pollutant, id = 1:332){
    mean_vector <- c()
    files <- list.files(directory)
    for (index in id) {
        data <- read.csv(pastehea(directory, "/", files[index], sep=""))
        cleaned_data <- data[which(!is.na(data[, pollutant])), pollutant]
        mean_vector <- c(mean_vector, cleaned_data)
    }
    mean(mean_vector)
}