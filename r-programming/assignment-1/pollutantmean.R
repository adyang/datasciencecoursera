pollutantmean <- function(directory, pollutant, id = 1:332) {
  total <- c(0, 0)
  for (i in id) {
    data <- read(i, directory)
    sumCount <- collectSumCount(pollutant, data)
    total <- total + sumCount 
  }
  total[1]/total[2]
}

read <- function(i, directory) {
  filename <- paste(sprintf("%03d", i), ".csv", sep = "")
  read.csv(file.path(directory, filename))
}

collectSumCount <- function(pollutant, data) {
  pollutantData <- data[,pollutant]
  valueSum <- sum(pollutantData, na.rm=TRUE)
  count <- sum(!is.na(pollutantData))
  c(valueSum, count)
}