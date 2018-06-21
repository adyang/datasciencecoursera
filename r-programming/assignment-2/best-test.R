runTests <- function() {
  source("test-util.R")
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