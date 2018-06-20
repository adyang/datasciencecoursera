assertEquals <- function (expected, actual) {
  if (expected != actual)
    stop(paste("Assertion Failed, expected:", "[", expected, "]", "but actual:", "[", actual, "]"))
}

assertStop <- function (func, stopMessage) {
  tryCatch({
    func()
    stop("Expected Stop Message but got none.")
  },error=function (cond) {
      if (conditionMessage(cond) != stopMessage) {
        stop(paste("Incorrect Stop Message,",
                   "expected:", "[", stopMessage, "]",
                   "but actual:", "[", conditionMessage(cond), "]"))
      }
    })
}

runTests <- function() {
  source("best.R")
  
  assertEquals("CYPRESS FAIRBANKS MEDICAL CENTER", best("TX", "heart attack"))
  assertEquals("FORT DUNCAN MEDICAL CENTER", best("TX", "heart failure"))
  assertEquals("JOHNS HOPKINS HOSPITAL, THE", best("MD", "heart attack"))
  assertEquals("GREATER BALTIMORE MEDICAL CENTER", best("MD", "pneumonia"))
  
  assertStop(function() best("BB", "heart attack"), "invalid state")
  assertStop(function() best("NY", "hert attack"), "invalid outcome")
  
  print("All Tests Passed!")  
}

runTests()