best <- function(state, outcome) {
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="NA", stringsAsFactors = FALSE)
  
  # Convert outcome columns to numeric
  outcomedata[,11] <- as.numeric(outcomedata[,11])
  outcomedata[,17] <- as.numeric(outcomedata[,17])
  outcomedata[,23] <- as.numeric(outcomedata[,23])
  
  # Map outcomes to columns
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  ## Check that state and outcome are valid
  # Check if selected outcome is in outcomes list
  if(is.na(outcomes[outcome])==TRUE) stop('Invalid Outcome')
  
  # Make list of unique states
  all_states <- unique(outcomedata[,7])
  
  # Check if input state belongs to the list
  if( (state %in% all_states)==FALSE ) stop('Invalid state')
  
  ## At this stage collect the required data 
  # Make a vector with the list of colum names
  data_selected <- c("State"=7, "Outcome"=outcomes[outcome], "Hospital"=2)
  
  # Select subset of outcomedata
  mydatasubset <- outcomedata[,data_selected]
  names(mydatasubset)<-c("State", "Outcome", "Hospital")
  
  # Remove all rows which have NA in first column corresponding to outcome
  newsubset <- subset(mydatasubset, !is.na(mydatasubset[2]))
  
  # Sort by state and then by Outcome
  tmpdf<-newsubset[order(newsubset$State, newsubset$Outcome, newsubset$Hospital),]
  
  # Split tmpdf by input state and collect output in another data frame
  tmpdf2<-split(tmpdf, tmpdf$State)[[state]]
  tmpdf2
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  head(tmpdf2,1)[,3]
}
