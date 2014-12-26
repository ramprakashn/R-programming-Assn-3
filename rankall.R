rankall <- function(outcome, num = "best") {
  
  # reads outcome file
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  
  # extracts the states list using the attr-levels for the factor
  states_array <- attr(outcome_data[,7], "levels")
  # sum condition will be 1 if a state in the list is available
  #if (sum(states_array == state) == 0) {stop("invalid state")} 
  
  outcomes_array <- c("heart attack", "heart failure", "pneumonia")
  if (sum(outcomes_array == outcome) == 0) {stop("invalid outcome")}
  
  if(outcome == "heart attack") {
    mcol <- 11
  } else if(outcome == "heart failure")  {
    mcol <- 17
  } else {
    mcol <- 23
  }
  outcome_subset <- outcome_data[, c(2, 7, mcol)] 
  out_df <- data.frame()
  
  for (i in seq_len(length(states_array)))
    {
      
      # extract state specific data
      ss_data <- outcome_subset[outcome_subset[,2] == states_array[i],c(1,3)]
  
      # extract complete cases of state specific data
      ss_data <- ss_data[complete.cases(ss_data),]
      
      if (num == "best") {rank_num <- 1}
      else if (num == "worst") {rank_num <- nrow(ss_data)}
      else {rank_num <- num}
      
      if (rank_num > nrow(ss_data)) 
        {
        out_df[i,1] <- "NA"
        }
      else  
       {
        ss_data[,2] <- as.numeric(ss_data[,2])
        ss_data[,1] <- as.character(ss_data[,1])
        ss_data <- ss_data[order(ss_data[,2], ss_data[,1]),]
        out_df[i,1] <- ss_data[rank_num,1]
        drop(ss_data)
      }
      out_df[i,2] <- states_array[i]
  }
  colnames(out_df) <- c("hospital", "state")
  out_df 
}
