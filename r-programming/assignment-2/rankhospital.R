validateState <- function(state, data) {
  if (!(state %in% data$State))
    stop("invalid state")
}

validateOutcome <- function(outcome, mortalityCols) {
  if (!(outcome %in% names(mortalityCols)))
    stop("invalid outcome")
}

parseNum <- function(num, lastRow) {
  wordNums <- list("best"=1, "worst"=lastRow)
  if (num %in% names(wordNums))
    return(wordNums[[num]])
  else if (is.numeric(num))
    return(num)
  else
    stop("invalid num")
}

sortRowsByMortality <- function(data, state, mortCol) {
  availData <- data[data[[mortCol]] != "Not Available",]
  availData[,mortCol] <- as.numeric(availData[, mortCol])
  dataInState <- availData[availData$State == state,]
  sortedRows <- dataInState[order(dataInState[[mortCol]], dataInState$Hospital.Name),]
  sortedRows
}

rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  mortalityCols <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  validateState(state, data)
  validateOutcome(outcome, mortalityCols)
  
  sortedRows <- sortRowsByMortality(data, state, mortalityCols[[outcome]])
  selectedRank <- parseNum(num, nrow(sortedRows))
  sortedRows[selectedRank, "Hospital.Name"]
}