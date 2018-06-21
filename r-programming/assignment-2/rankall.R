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


sortRowsByMortality <- function(data, mortCol) {
  availData <- data[data[[mortCol]] != "Not Available",]
  availData[,mortCol] <- as.numeric(availData[, mortCol])
  sortedRows <- availData[order(availData[[mortCol]], availData$Hospital.Name),]
  sortedRows
}

findHospitalInState <- function(data, mortCol, num) {
  sortedRows <- sortRowsByMortality(data, mortCol)
  selectedRank <- parseNum(num, nrow(sortedRows))
  selectedHospital <- sortedRows[selectedRank, "Hospital.Name"]
  data.frame(hospital=selectedHospital, state=data[1, "State"])
}

rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  mortalityCols <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  validateOutcome(outcome, mortalityCols)
  
  stateGroups <- split(data, data$State)
  stateHospitalGroups <- lapply(stateGroups, findHospitalInState, mortalityCols[[outcome]], num)
  do.call(rbind, stateHospitalGroups)
}