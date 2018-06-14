corr <- function(directory, threshold = 0) {
  result <- numeric()
  for (filename in list.files(directory, pattern="*.csv", full.names = TRUE)) {
    data <- read.csv(filename)
    if (sum(complete.cases(data)) > threshold) {
      completeData <- data[complete.cases(data),]
      result <- c(result, cor(x = completeData$sulfate, y = completeData$nitrate)) 
    }
  }
  result
}
