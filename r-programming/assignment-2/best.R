best <- function (state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  mortalityCols <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  if (!(state %in% data$State))
    stop("invalid state")
  if (!(outcome %in% names(mortalityCols)))
    stop("invalid outcome")
  
  mortCol <- mortalityCols[[outcome]]
  data[,mortCol] <- as.numeric(data[, mortCol])
  mortalityRates <- data[, mortCol]
  dataInState <- data[data$State == state & !is.na(data[,mortCol]),]
  bestRows <- dataInState[dataInState[,mortCol] == min(dataInState[,mortCol]),]
  best <- head(bestRows[order(bestRows$Hospital.Name),], 1)
  best$Hospital.Name
}