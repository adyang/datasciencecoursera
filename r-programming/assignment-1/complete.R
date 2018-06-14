complete <- function(directory, id = 1:332) {
  result <- data.frame("id"=integer(), "nobs"=integer())
  for (i in id) {
    data <- read(i, directory)
    nobs <- sum(complete.cases(data))
    result <- rbind(result, data.frame("id"=i, "nobs"=nobs))
  }
  result
}

read <- function(i, directory) {
  filename <- paste(sprintf("%03d", i), ".csv", sep = "")
  read.csv(file.path(directory, filename))
}