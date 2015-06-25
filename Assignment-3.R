best <- function(state, outcome) {
  ## Read outcome data
  outcome.data <- read.csv("outcome-of-care-measures.csv")
  str(outcome.data[outcome.data==state])
  ## Check that state and outcome are valid
  outcome.filtered.state <- tryCatch(outcome.data$State[outcome.data$State==state],warning=function(){stop("invalid state")})
  if(outcome!="heart attack"|outcome!="heart failure"|outcome!="pneumonia")stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
