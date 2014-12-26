best <- function ( state, outcome)
{
  outcome_data <- read.csv("outcome-of-care-measures.csv") # reads outcome file
  
  states_array <- attr(outcome_data[,7], "levels")  # extracts the states list using the attr-levels for the factor
  
  if (sum(states_array == state) == 0) {stop("invalid state")} # sum condition will be 1 if a state in the list is available
  
  outcomes_array <- c("heart attack", "heart failure", "pneumonia")
  
  if (sum(outcomes_array == outcome) == 0) {stop("invalid outcome")}
  
  if(outcome == "heart attack") {
    mcol <- 11
  } else if(outcome == "heart failure")  {
    mcol <- 17
  } else {
    mcol <- 23
  }
  
  # extract state specific data
  ss_data <- outcome_data[outcome_data[,7] == state,c(2,mcol)]
  ss_data[,2] <- as.numeric(ss_data[,2])
  so_data <- ss_data[ss_data[,2] == min(ss_data[,2]),1]
  so_data <- as.character(sort(so_data))
  so_data[1]
}
