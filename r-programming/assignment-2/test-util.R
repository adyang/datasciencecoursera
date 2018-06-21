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
