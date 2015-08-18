best <- function(state, outcome) {
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  
  index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  data[,index] <- as.numeric(data[,index])
  data <- na.omit(data)
  
 
  states <- table(data$State)
  if (!state %in% names(states)) { 
    stop("invalid state")
  }
  
 #sort it by outcome and hospital name.
  slice <- subset(data, State==state)
  slice <- slice[order(slice[,index], na.last=TRUE),2]
  slice <- na.omit(slice)
  
  #Get hospital name with the lowest 30-day mortality rate.
  slice[1]
}