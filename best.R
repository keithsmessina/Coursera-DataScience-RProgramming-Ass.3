best <- function(state, outcome) {
  ## Read outcome data
  care.data <- read.csv("outcome-of-care-measures.csv")

  ## Check that state is valid
  if(!(state %in% care.data$State))stop("invalid state")
  
  ## Create a vector with all of the valid outcomes options
  outcome.vector <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check if input outcome value exists as a valid outcome option
  if(!(outcome %in% outcome.vector))stop("invalid outcome")

  
  ## Return hospital name in that state with lowest 30-day death
  ## rate

  ## Convert the outcome input into the function into the 
  ## data frame column number that matches the desired outcome
  if(outcome == "heart attack") {
    outcome.match <- 11
  } else if(outcome == "heart failure") {
    outcome.match <- 17
  } else {
    outcome.match <- 23
  }

  ## Save a subset of the data frame, saving just the relevant
  ## columns (Hospital Name, Outcome) with the state matching the
  ## input state
  care.data.filtered <- care.data[care.data$State==state,c(2,outcome.match)]
  
  ## CHange the outcome data into a numeric so it can be sorted
  care.data.filtered[,2] <- suppressWarnings(as.numeric(as.character(care.data.filtered[,2])))

  ## Remove NAs from the data frame
  care.data.filtered <- care.data.filtered[!is.na(care.data.filtered[,2]),]

  ## Sort the dataframe by the outcome column
  care.data.filtered <- care.data.filtered[order(care.data.filtered[,2]),]

  tied <- match(care.data.filtered[,2],care.data.filtered[,2])
  tied <- tied[tied == 1]
  if(length(tied)>1) {
    care.data.filtered <- care.data.filtered[c(1:length(tied)),]
    care.data.filtered <- care.data.filtered[order(care.data.filtered[,1]),]
  }
  head(as.character(care.data.filtered$Hospital.Name),1)
}
