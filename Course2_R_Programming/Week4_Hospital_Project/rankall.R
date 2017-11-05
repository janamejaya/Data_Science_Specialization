hospital_name_function <- function(x){
 #if(num=="worst"){
    x[as.integer(count(x)),3]
#  }
 # else x[num,3]
  #x[num,3]
}

rankall <- function(outcome, num = "best") {
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
  all_states <- sort(unique(outcomedata[,7]))
  
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
  tmpdf2<-split(tmpdf, tmpdf$State)
  tmpdf2
  
  if(num!="worst"){
    if(num=="best"){
      num <- 1
    }
    tmpdf3<-sapply(tmpdf2,function(data) data[num,3])
  }
  else {
        tmpdf3<-sapply(tmpdf2, function(x) x[as.integer(count(x)),3])
       }


  # Apply sapply and collect resulting list in tmpdf3
#  tmpdf3<-sapply(tmpdf2,function(data,num) data[indexval,3])
  #tmpdf3 <- sapply(tmpdf2, tmpfunction(data,num))
   #sapply(tmpdf2, tmpfunction(data, nrow))
   #tmpdf2
    
  # Construct data frame with the name of the hospital ranked num, state 
  # row name equal to state, column names of Hospital and state
  data.frame(Hospital=tmpdf3, state=all_states, row.names=all_states)
}