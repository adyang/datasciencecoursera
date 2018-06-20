validateState <- function(state, data) {
  if (!(state %in% data$State))
    stop("invalid state")
}

validateOutcome <- function(outcome, mortalityCols) {
  if (!(outcome %in% names(mortalityCols)))
    stop("invalid outcome")
}

findBestRows <- function(data, state, mortCol) {
  availData <- data[data[[mortCol]] != "Not Available",]
  availData[,mortCol] <- as.numeric(availData[, mortCol])
  dataInState <- availData[availData$State == state,]
  bestRows <- dataInState[dataInState[,mortCol] == min(dataInState[,mortCol]),]
  bestRows
}

best <- function (state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  mortalityCols <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)

  validateState(state, data)
  validateOutcome(outcome, mortalityCols)
  
  bestRows <- findBestRows(data, state, mortalityCols[[outcome]])
  
  best <- head(bestRows[order(bestRows$Hospital.Name),], 1)
  best$Hospital.Name
}