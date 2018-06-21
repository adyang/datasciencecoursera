runTests <- function() {
  source("test-util.R")
  source("rankhospital.R")

  assertEquals("CYPRESS FAIRBANKS MEDICAL CENTER", rankhospital("TX", "heart attack", 1))
  assertEquals("FORT DUNCAN MEDICAL CENTER", rankhospital("TX", "heart failure", 1))
  assertEquals("JOHNS HOPKINS HOSPITAL, THE", rankhospital("MD", "heart attack", 1))
  assertEquals("GREATER BALTIMORE MEDICAL CENTER", rankhospital("MD", "pneumonia", 1))
  
  assertEquals("DETAR HOSPITAL NAVARRO", rankhospital("TX", "heart failure", 4))
  assertEquals(TRUE, is.na(rankhospital("TX", "heart failure", 5000)))

  assertEquals("GREATER BALTIMORE MEDICAL CENTER", rankhospital("MD", "pneumonia", "best"))
  assertEquals("GREATER BALTIMORE MEDICAL CENTER", rankhospital("MD", "pneumonia"))
  assertEquals("HARFORD MEMORIAL HOSPITAL", rankhospital("MD", "heart attack", "worst"))
  
  assertStop(function() rankhospital("BB", "heart attack", 1), "invalid state")
  assertStop(function() rankhospital("NY", "hert attack", 1), "invalid outcome")
  assertStop(function() rankhospital("NY", "heart attack", "invalidNum"), "invalid num")

  print("All Tests Passed!")  
}

runTests()